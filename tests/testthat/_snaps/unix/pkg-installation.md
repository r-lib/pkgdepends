# create_lockfile

    Code
      lock$packages
    Output
         ref package version     type direct binary dependencies
      1 pkg1    pkg1   1.0.0 standard  FALSE  FALSE             
      2 pkg2    pkg2   1.0.0 standard  FALSE  FALSE         pkg1
      3 pkg3    pkg3   1.0.0 standard   TRUE  FALSE         pkg2
                           deps vignettes needscompilation metadata.RemoteType
      1                    NULL     FALSE            FALSE            standard
      2 pkg1, depends, pkg1, ,      FALSE            FALSE            standard
      3 pkg2, depends, pkg2, ,      FALSE            FALSE            standard
        metadata.RemotePkgRef metadata.RemoteRef   metadata.RemoteRepos
      1                  pkg1               pkg1 http://127.0.0.1:<port>
      2                  pkg2               pkg2 http://127.0.0.1:<port>
      3                  pkg3               pkg3 http://127.0.0.1:<port>
        metadata.RemotePkgPlatform metadata.RemoteSha
      1                     source              1.0.0
      2                     source              1.0.0
      3                     source              1.0.0
                                                                                                                        sources
      1 http://127.0.0.1:<port>/src/contrib/pkg1_1.0.0.tar.gz, http://127.0.0.1:<port>/src/contrib/Archive/pkg1/pkg1_1.0.0.tar.gz
      2 http://127.0.0.1:<port>/src/contrib/pkg2_1.0.0.tar.gz, http://127.0.0.1:<port>/src/contrib/Archive/pkg2/pkg2_1.0.0.tar.gz
      3 http://127.0.0.1:<port>/src/contrib/pkg3_1.0.0.tar.gz, http://127.0.0.1:<port>/src/contrib/Archive/pkg3/pkg3_1.0.0.tar.gz
                               target platform rversion directpkg
      1 src/contrib/pkg1_1.0.0.tar.gz   source        *     FALSE
      2 src/contrib/pkg2_1.0.0.tar.gz   source        *     FALSE
      3 src/contrib/pkg3_1.0.0.tar.gz   source        *      TRUE
                                                                  sha256 filesize
      1 <sha>      100
      2 <sha>      100
      3 <sha>      100
                          dep_types params install_args repotype
      1 Depends, Imports, LinkingTo   NULL         NULL     cran
      2 Depends, Imports, LinkingTo   NULL         NULL     cran
      3 Depends, Imports, LinkingTo   NULL         NULL     cran

