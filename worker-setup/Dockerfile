FROM matrix-worker-base
#########################################################################
# matrix.hho specific environment configuration

# wrapper scripts
ADD ./xghc-7.x /opt/ghc/7.0.4/bin/xghc
ADD ./xghc-7.x /opt/ghc/7.4.2/bin/xghc
ADD ./xghc-7.x /opt/ghc/7.6.3/bin/xghc
ADD ./xghc-7.x /opt/ghc/7.8.4/bin/xghc
ADD ./xghc-7.x /opt/ghc/7.10.3/bin/xghc
ADD ./xghc-8.x /opt/ghc/8.0.2/bin/xghc
ADD ./xghc-8.x /opt/ghc/8.2.2/bin/xghc
ADD ./xghc-8.x /opt/ghc/8.4.4/bin/xghc
ADD ./xghc-8.x /opt/ghc/8.6.5/bin/xghc
RUN sed -i 's|"$PKGCONF" ${1+"$@"}|"$PKGCONF" +RTS -M1G -RTS ${1+"$@"}|g' /opt/ghc/*/bin/ghc-pkg-[78]*.*

# user setup
RUN groupadd -r matrix && useradd -d /matrix -r -m -g matrix matrix && chown 0:0 /matrix && mkdir /matrix/.cabal && chown matrix:matrix /matrix/.cabal && mkdir /matrix/log && chown matrix:matrix /matrix/log
ENV PATH=/matrix/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
ADD ./worker.conf /matrix/worker.conf
ADD ./bin/ /matrix/bin/
USER matrix
WORKDIR /matrix
RUN cabal user-config init -a 'http-transport: plain-http' && sed -i 's,http://hackage,http://hackage-origin,g' .cabal/config
RUN cabal v1-update -v
CMD ["/matrix/bin/matrix-worker", "/matrix/worker.conf"]
