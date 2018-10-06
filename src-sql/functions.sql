----------------------------------------------------------------------------
-- minor helpers

CREATE OR REPLACE FUNCTION max_ptime() RETURNS int AS $$
SELECT max(ptime) FROM idxstate;
$$ LANGUAGE SQL ;

CREATE OR REPLACE FUNCTION unix_now() RETURNS int AS $$
BEGIN RETURN extract(epoch from now()); END;
$$ LANGUAGE plpgsql;

-- Mostly useful for sorting (and for functional indices)
-- TODO: use custom type
-- todo: verify >= 0, and >= {0}
CREATE OR REPLACE FUNCTION as_ver(text) RETURNS int[] AS $$
SELECT string_to_array($1,'.')::int[];
$$ LANGUAGE SQL IMMUTABLE STRICT;

----------------------------------------------------------------------------
-- trigger functions

CREATE OR REPLACE FUNCTION table_event_notify() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  -- PERFORM pg_notify('table_event', json_build_object('table', TG_TABLE_NAME, 'op', TG_OP)::text);
  PERFORM pg_notify('table_event', TG_TABLE_NAME::text);
  RETURN NEW;
END;
$$;

---

CREATE OR REPLACE FUNCTION version_trigger_proc() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  NEW.pvera := string_to_array(NEW.pver,'.')::int[];
RETURN NEW;
END;
$$;

----

CREATE OR REPLACE FUNCTION solution_fail_insert_trigger() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO pname_max_ptime(pname,ptime)
    VALUES(NEW.pname,NEW.ptime)
    ON CONFLICT(pname)
    DO UPDATE SET ptime = GREATEST(pname_max_ptime.ptime, EXCLUDED.ptime);

  INSERT INTO pname_ptimes(pname,ptime)
    VALUES(NEW.pname,NEW.ptime)
    ON CONFLICT(pname,ptime) DO NOTHING;

  RETURN NEW;
END;
$$;

----

CREATE OR REPLACE FUNCTION solution_insert_trigger() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO pname_max_ptime(pname,ptime)
    VALUES((SELECT pname FROM iplan_job j WHERE j.jobid = NEW.jobid), NEW.ptime)
    ON CONFLICT(pname)
    DO UPDATE SET ptime = GREATEST(pname_max_ptime.ptime, EXCLUDED.ptime);

  INSERT INTO pname_ptimes(pname,ptime)
    VALUES((SELECT pname FROM iplan_job j WHERE j.jobid = NEW.jobid), NEW.ptime)
    ON CONFLICT(pname,ptime) DO NOTHING;

  RETURN NEW;
END;
$$;

----------------------------------------------------------------------------
-- Sync uncached "solution" rows to "solution_rev" and mark stale
-- affected rows of "solution_span"

CREATE OR REPLACE FUNCTION job_trans_xunits(jid UUID) RETURNS SETOF UUID AS $$
  WITH RECURSIVE t(xunitid) AS (
    SELECT unnest(units)
        FROM iplan_job
        WHERE jobid = jid
    UNION
    SELECT d.child
        FROM iplan_comp_dep d, t
        WHERE d.parent = t.xunitid
    )
    SELECT DISTINCT xunitid FROM t ORDER BY xunitid;
$$ LANGUAGE SQL;

----

CREATE OR REPLACE FUNCTION sync_solution_rev (workcnt int)
RETURNS int AS $$
DECLARE
  rec RECORD;
  rowcnt int := 0;
BEGIN
  FOR rec IN SELECT jobid,ptime FROM solution
                                WHERE not cached AND ptime <= (SELECT ptime_watermark FROM globals)
                                ORDER BY ptime ASC
                                LIMIT workcnt
  LOOP
    -- RAISE NOTICE 'updating jobid=% ptime=%', rec.jobid, rec.ptime;
    WITH
      sol_rev_tmp AS (SELECT job_trans_xunits(rec.jobid) xunitid),

      newrevs AS (
        -- lookup & insert new (xunitid,prev) tuples
        INSERT INTO solution_rev
        SELECT xunitid,max(prev)
           FROM sol_rev_tmp t JOIN iplan_unit u USING (xunitid)
                              JOIN pkgindex i USING (pname,pver)
           WHERE pkind <> 'builtin' AND ptime <= rec.ptime GROUP BY xunitid
           ON CONFLICT DO NOTHING
           RETURNING xunitid
      )

      UPDATE solution_span ss SET vstale='t' FROM newrevs nr WHERE ss.xunitid = nr.xunitid;

    -- finally mark solution (i.e. the jobid/ptime pair) as "done"
    UPDATE solution SET cached = 't' WHERE jobid = rec.jobid AND ptime = rec.ptime;
    rowcnt := rowcnt + 1;
  END LOOP;

  RETURN rowcnt;
END ;
$$ LANGUAGE plpgsql;

----------------------------------------------------------------------------
-- check invariant of idxranges (as used in "solution_span" table)

CREATE OR REPLACE FUNCTION check_idxranges(ira int4range[])
RETURNS bool AS $$
DECLARE
  cur  int4range;
  prev int4range := NULL;
BEGIN
  IF ira is NULL THEN RETURN 'f'; END IF;

  FOREACH cur in ARRAY ira
  LOOP
    IF isempty(cur) THEN RETURN 'f'; END IF;

    IF prev is NULL THEN -- first elem
       IF lower_inf(cur) THEN RETURN 'f'; END IF;
    ELSE -- non-first elems
       IF not (prev << cur) or (prev -|- cur) THEN RETURN 'f'; END IF;
    END IF;

    prev := cur;
  END LOOP;

  RETURN 't';
END;
$$ IMMUTABLE LANGUAGE plpgsql;

/*

SELECT check_idxranges('{}'::int4range[]);

SELECT check_idxranges('{"[1,4)","[5,7)"}'::int4range[]);

SELECT check_idxranges('{"[0,)"}'::int4range[]);

SELECT check_idxranges('{"[,10)"}'::int4range[]);


SELECT check_idxranges('{"[1,4)","[5,)"}'::int4range[]);

SELECT check_idxranges('{"[5,7)","[1,4)"}'::int4range[]);

SELECT check_idxranges('{"[1,4)","[4,7)"}'::int4range[]);

SELECT check_idxranges('{"[1,4)","[3,7)"}'::int4range[]);

*/
