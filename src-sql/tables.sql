
-- TODO

-- add 'ON DELETE CASCADE's

----------------------------------------------------------------------------


-- https://www.postgresql.org/message-id/CADCw5QZctww5Q1MiTEx9OtOhT4g421Ftds%3D1MtTepqM2hLnrCQ%40mail.gmail.com

/* Needs db admin privs */

CREATE OPERATOR CLASS _uuid_ops DEFAULT
  FOR TYPE _uuid USING gin AS
  OPERATOR 1 &&(anyarray, anyarray),
  OPERATOR 2 @>(anyarray, anyarray),
  OPERATOR 3 <@(anyarray, anyarray),
  OPERATOR 4 =(anyarray, anyarray),
  FUNCTION 1 uuid_cmp(uuid, uuid),
  FUNCTION 2 ginarrayextract(anyarray, internal, internal),
  FUNCTION 3 ginqueryarrayextract(anyarray, internal, smallint, internal, internal, internal, internal),
  FUNCTION 4 ginarrayconsistent(internal, smallint, anyarray, integer, internal, internal, internal, internal),
  STORAGE uuid;


-- NB:
--
-- integer  -> Int32
-- smallint -> Int16
-- bigint   -> Int64
-- uuid     -> 128bit
-- enums    -> 32bit

-- The "unit type" (useful as PK of single-row tables, corresponds to Haskell's ())
CREATE TYPE unit AS enum('unit');

-- encodes valid(!) versions both as string & array
CREATE TABLE version (
-- TODO: add new pver_id
    pver text PRIMARY KEY,
    -- pvera gives us lexicographic ordering
    pvera int[] NOT NULL CHECK ((pvera <> '{}') AND (0 <= all(pvera)))
);

DROP TRIGGER   version_trigger ON version;
CREATE TRIGGER version_trigger BEFORE INSERT OR UPDATE ON version
  FOR EACH ROW EXECUTE PROCEDURE version_trigger_proc();

----------------------------------------------------------------------------

CREATE TABLE pkgname (
    pname text PRIMARY KEY,
    pname_id int UNIQUE NOT NULL -- TODO: make PK
);

-- TODO: rename to pkgrel or pkgnamever
CREATE TABLE pkgver (
    pname   text     REFERENCES pkgname(pname),
    pver    text     REFERENCES version(pver),
    -- TODO: sha256
    PRIMARY KEY (pname, pver)
);

-- packages with broken/missing tarballs
CREATE TABLE pkg_blacklist (
    pname   text,
    pver    text,
    PRIMARY KEY(pname,pver),
    FOREIGN KEY(pname,pver) REFERENCES pkgver(pname,pver),
    ctime    int NOT NULL DEFAULT unix_now() -- creation time of this row
);

/* special builtin entries we need to add on init; done by controller

INSERT INTO pkgname VALUES ('rts');
INSERT INTO version VALUES ('1.0');
INSERT INTO pkgver  VALUES ('rts','1.0');

*/

CREATE TABLE idxstate (
    ptime int PRIMARY KEY -- TODO: bigint
);

----------------------------------------------------------------------------

-- global parameters
CREATE TABLE globals (
    unit unit PRIMARY KEY default 'unit',
    ptime_watermark int REFERENCES idxstate(ptime) DEFAULT max_ptime()
);

INSERT INTO globals(unit) VALUES (default) ON CONFLICT DO NOTHING;

----------------------------------------------------------------------------

-- TODO: add table for ver-pref events
CREATE TABLE pkgindex (
    pname   text,
    pver    text,
    prev    int,
    PRIMARY KEY (pname, pver, prev),
    ------------------------------------------------------------
    ptime   int     NOT NULL REFERENCES idxstate(ptime), -- posix-time
    powner  text    NOT NULL, -- redundant; fixme
    powner_id int   NOT NULL REFERENCES owner(owner_id),
    FOREIGN KEY (pname, pver) REFERENCES pkgver (pname, pver)
);

CREATE INDEX ON pkgindex(pname);
CREATE INDEX ON pkgindex(powner);
CREATE INDEX ON pkgindex(pname,pver);
CREATE INDEX ON pkgindex(ptime);

DROP TRIGGER   pkgindex_event_notify ON pkgindex;
CREATE TRIGGER pkgindex_event_notify AFTER INSERT OR UPDATE ON pkgindex
  EXECUTE PROCEDURE table_event_notify();

----------------------------------------------------------------------------

CREATE TABLE owner (
  owner_id int PRIMARY KEY,
  name     text NOT NULL UNIQUE
);

----------------------------------------------------------------------------

CREATE TABLE hscompiler (
    compiler text PRIMARY KEY
    ui_ver   text UNIQUE, -- manual mapping
);

INSERT INTO hscompiler (compiler,ui_ver) VALUES ('ghc-7.8.4','7.8');
INSERT INTO hscompiler (compiler,ui_ver) VALUES ('ghc-7.10.3','7.10');
INSERT INTO hscompiler (compiler)        VALUES ('ghc-8.0.1');
INSERT INTO hscompiler (compiler,ui_ver) VALUES ('ghc-8.0.2', '8.0');
INSERT INTO hscompiler (compiler,ui_ver) VALUES ('ghc-7.4.2', '7.4');
INSERT INTO hscompiler (compiler,ui_ver) VALUES ('ghc-7.6.3', '7.6');

----------------------------------------------------------------------------

CREATE TABLE pname_tag (
    pname    text     NOT NULL REFERENCES pkgname(pname),
    tagname  text     NOT NULL,
    PRIMARY KEY (pname, tagname)
);

CREATE INDEX ON pname_tag(pname);
CREATE INDEX ON pname_tag(tagname);

----------------------------------------------------------------------------

CREATE TABLE queue (
    pname    text    NOT NULL REFERENCES pkgname(pname),
    prio     int     NOT NULL,
    modified timestamptz NOT NULL DEFAULT now(),
    ptime    int     NOT NULL REFERENCES idxstate(ptime) DEFAULT max_ptime(),
    PRIMARY KEY (pname, ptime) -- for now
);

DROP TRIGGER   queue_event_notify ON queue;
CREATE TRIGGER queue_event_notify AFTER INSERT OR UPDATE ON queue
  EXECUTE PROCEDURE table_event_notify();

----------------------------------------------------------------------------

CREATE TYPE wstate AS enum('idle','init','solve','build-deps','build','done','error');

CREATE TABLE worker (
    wid      int    PRIMARY KEY,
    mtime    int    NOT NULL DEFAULT unix_now(), -- last time this row was updated
    wstate   wstate NOT NULL,

    pname    text   NULL,
    pver     text   NULL,
    FOREIGN KEY(pname,pver) REFERENCES pkgver(pname,pver),
    ptime    int    NULL REFERENCES idxstate(ptime),
    compiler text   NULL REFERENCES hscompiler(compiler) -- TODO: os/arch
);

----------------------------------------------------------------------------

CREATE TYPE ipkind AS enum('builtin','global','local');

CREATE TYPE ipstatus AS enum('ok','fail','fail_deps');
-- 'fail_deps' as soon as any dep is fail or fail_deps

-- CREATE TYPE deptype AS enum('lib','exe');


-- root-goal for which there's a plan.json
CREATE TABLE iplan_job (
    jobid    uuid        PRIMARY KEY, -- hashed over units

    -- slightly redundant
    pname    text        NOT NULL,
    pver     text        NOT NULL,
    FOREIGN KEY(pname,pver) REFERENCES pkgver(pname,pver),
    compiler text        NOT NULL REFERENCES hscompiler(compiler), -- TODO: os/arch

    plan     json        NOT NULL, -- original plan.json -- TODO: move to separate "blob" table

    -- secondary PK
    units    uuid[]      NOT NULL, -- CHECK ALL REFERENCES iplan_unit(xunitid)
    -- should never ref 'global' units
    -- edge case however: builtins
    ctime    int         NOT NULL DEFAULT unix_now() -- creation time of this row
);

CREATE INDEX ON iplan_job USING GIN (units);
CREATE INDEX ON iplan_job(pname,pver);

-- represents a unit like in Cabal
CREATE TABLE iplan_unit (
    xunitid  uuid        PRIMARY KEY, -- really unique unitid generated via hash from other fields

    -- external "weak primary key"
    unitid   text        NOT NULL, -- original unit-id, not necessarily globally unique for non-global pkgs
    compiler text        NOT NULL REFERENCES hscompiler(compiler), -- TODO: os/arch
    pkind    ipkind      NOT NULL, -- builtin/global/local

    pname    text        NOT NULL,
    pver     text        NOT NULL,
    FOREIGN KEY(pname,pver) REFERENCES pkgver(pname,pver),

    flags    json        NOT NULL, -- for generating constraints
    -- cnames text[]     NOT NULL, -- paranoia

    bstatus  ipstatus    NULL, -- may be NULL since we don't use --keep-going yet
    logmsg   text, -- NULL for 'fail_deps' status, as well as for builtin libs

    dt       real,
    ctime    int         NOT NULL DEFAULT unix_now() -- creation time of this row
);

CREATE index ON iplan_unit(pname,pver);

/* (currently unused!)

-- at some point we will be able to merge iplan_pkg w/ iplan_comp
-- when per-component will support also custom-btypes
CREATE TABLE iplan_comp (
    xunitid  uuid NOT NULL REFERENCES iplan_unit(xunitid),
    -- TODO: split cname into ctype & cid?
    cname    text NOT NULL, -- lib / exe:... / lib:foo
    PRIMARY KEY(xunitid,cname),
    ------------------------------------------------------------

    -- maybe merge these?
    lib_deps uuid[], -- CHECK ALL REFERENCES iplan_unit(xunitid)
    exe_deps uuid[] -- dito
);

CREATE INDEX ON iplan_comp USING GIN (lib_deps);
CREATE INDEX ON iplan_comp USING GIN (exe_deps);

CREATE VIEW unit_dep AS
 SELECT DISTINCT c.xunitid AS parent,
    unnest(c.lib_deps || c.exe_deps) AS child
   FROM iplan_comp c;

*/

-- convert iplan_comp into iplan_comp_dep
/*
INSERT INTO iplan_comp_dep(parent,cname,isExeDep,child) SELECT c.xunitid AS parent, cname, FALSE isExeDep, unnest(c.lib_deps) AS child FROM iplan_comp c;
INSERT INTO iplan_comp_dep(parent,cname,isExeDep,child) SELECT c.xunitid AS parent, cname, TRUE  isExeDep, unnest(c.exe_deps) AS child FROM iplan_comp c;
*/

CREATE TABLE iplan_comp_dep (
    parent   uuid    NOT NULL REFERENCES iplan_unit(xunitid) ON DELETE CASCADE,
    cname    text    NOT NULL,
    isExeDep boolean NOT NULL,
    child    uuid    NOT NULL REFERENCES iplan_unit(xunitid), -- NB: *NO* deletion cascade
    PRIMARY KEY (parent, cname, isExeDep, child)
);

CREATE INDEX ON iplan_comp_dep (parent);
CREATE INDEX ON iplan_comp_dep (child);

----------------------------------------------------------------------------

-- TODO: allow adding constraints to 'solution'/'solution_fail'?
--       probably better: have constraints be ephemeral property of
--       requests (aka queue entries)

-- Maps input state/environment to (computed/known) solutions of the
-- CSP (aka "install-plans")
--
-- This is not an exhaustive enumeration of all possible solutions
--
--
CREATE TABLE solution (
    ptime int            REFERENCES idxstate(ptime),
    jobid uuid           REFERENCES iplan_job(jobid),
    PRIMARY KEY(ptime,jobid),

    dt    real,
    ctime int NOT NULL DEFAULT unix_now() -- creation time of this row
    cached bool NOT NULL DEFAULT 'f'
);

CREATE INDEX ON solution(cached);

DROP TRIGGER solution_insert_trigger ON solution;
CREATE TRIGGER solution_insert_trigger AFTER INSERT ON solution
  FOR EACH ROW EXECUTE PROCEDURE solution_insert_trigger();

-- ALTER TABLE solution ADD COLUMN cached boolean NOT NULL DEFAULT 'f';

-- ~~quite some overlap w/ iplan_pkg; consider subtyping~~ there is no iplan_pkg anymore
CREATE TABLE solution_fail (
    ptime    int         REFERENCES idxstate(ptime),
    compiler text        REFERENCES hscompiler(compiler), -- TODO: os/arch
    pname    text,
    pver     text,
    FOREIGN KEY(pname,pver) REFERENCES pkgver(pname,pver),
    PRIMARY KEY(ptime,pname,pver,compiler),
    ------------------------------------------------------------
    -- TODO: jobid uuid NULL REFERENCES iplan_job(jobid)
    solvererr text,
    solverlim int,
    dt    real,
    ctime int NOT NULL DEFAULT unix_now() -- creation time of this row
);

DROP TRIGGER solution_fail_insert_trigger ON solution_fail;
CREATE TRIGGER solution_fail_insert_trigger AFTER INSERT ON solution_fail
  FOR EACH ROW EXECUTE PROCEDURE solution_fail_insert_trigger();

CREATE TABLE solution_span (
    xunitid   uuid NOT NULL PRIMARY KEY REFERENCES iplan_unit(xunitid) ON DELETE CASCADE,
    valid     int4range[] NOT NULL CHECK(check_idxranges(valid)), -- ptime ranges for which 'xunitid' is *known* to be a solution
    vstale    bool NOT NULL, -- 'valid' is stale

    ptime0    int  NOT NULL REFERENCES idxstate(ptime) -- the earliest possible index-state where this *can* be a solution
               -- IOW, any time-ranges must be strictly right of `[,ptime0)` for this xunitid
);

-- encodes "there is (at least) one solution for xunitid and a specific prev"
CREATE TABLE solution_rev (
    xunitid   uuid NOT NULL REFERENCES iplan_unit(xunitid) ON DELETE CASCADE,
    prev      int  NOT NULL,
    PRIMARY KEY(xunitid,prev)
);

CREATE INDEX ON solution_rev(xunitid);

----------------------------------------------------------------------------
-- latest report by package-name cache
-- (NB: pname_max_ptime was formerly a VIEW)

CREATE TABLE pname_max_ptime (
   pname  text PRIMARY KEY   REFERENCES pkgname(pname),
   ptime  int  NOT NULL      REFERENCES idxstate(ptime)
);


BEGIN;

DELETE FROM pname_max_ptime;

INSERT INTO pname_max_ptime(pname,ptime)
 SELECT t.pname, max(t.ptime) AS ptime
   FROM ( SELECT iplan_job.pname,
            max(solution.ptime) AS ptime
           FROM solution
             JOIN iplan_job USING (jobid)
          GROUP BY iplan_job.pname
        UNION
         SELECT solution_fail.pname,
            max(solution_fail.ptime) AS ptime
           FROM solution_fail
          GROUP BY solution_fail.pname) t
  GROUP BY t.pname;

COMMIT;

----------------------------------------------------------------------------
-- generalization of pname_max_ptime
-- i.e. all ptimes for which we have at least one report cell
--
-- We could make `pname_max_ptime` a VIEW w/ neglible cost for
-- single-row queries; needs evaluation
--
--  CREATE VIEW pname_max_ptime AS SELECT pname, max(ptime) FROM pname_ptimes GROUP BY pname;
--
-- NB: pname_ptimes and pname_max_ptime are populated via trigger functions

CREATE TABLE pname_ptimes (
   pname  text REFERENCES pkgname(pname),
   ptime  int  REFERENCES idxstate(ptime),
   PRIMARY KEY(pname,ptime)
);


BEGIN;

DELETE FROM pname_ptimes;

INSERT INTO pname_ptimes(pname,ptime)
 SELECT t.pname, t.ptime
   FROM ( SELECT iplan_job.pname, solution.ptime FROM solution JOIN iplan_job USING (jobid)
          UNION
          SELECT solution_fail.pname, solution_fail.ptime FROM solution_fail ) t;

COMMIT;



/*

Reset plans

                       List of relations
 Schema |     Name      | Type  | Owner |  Size   | Description
--------+---------------+-------+-------+---------+-------------
 public | hscompiler    | table | hvr   | 48 kB   |
 public | idxstate      | table | hvr   | 3000 kB |
 public | iplan_comp    | table | hvr   | 2128 kB | 6343
 public | iplan_job     | table | hvr   | 34 MB   | 1636
 public | iplan_unit    | table | hvr   | 2656 kB | 5691
 public | pkgindex      | table | hvr   | 5976 kB |
 public | pkgname       | table | hvr   | 488 kB  |
 public | pkgver        | table | hvr   | 3656 kB |
 public | pname_tag     | table | hvr   | 56 kB   |
 public | queue         | table | hvr   | 48 kB   |
 public | solution      | table | hvr   | 208 kB  | 3361
 public | solution_fail | table | hvr   | 5736 kB |
 public | unit_dep      | view  | hvr   | 0 bytes |
 public | version       | table | hvr   | 480 kB  |


DELETE FROM solution;

DELETE FROM solution_fail;

DELETE FROM iplan_job;
DELETE FROM iplan_comp;
DELETE FROM iplan_unit;


*/
