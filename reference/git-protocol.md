# git protocol notes, for developers

Assumptions, they might be relaxed or checked for later:

- The server must speak the smart protocol, version 1 or 2. (Although I
  added some functions specifically for the dumb protocol, but they are
  pretty limited.)

- We use HTTP transport, not SSH.

- The server should have the `shallow` capability.

- The server should have the `filter` capability if protocol version 2.

- Only SHA-1 hashing is supported.

## Details

Improvements needed:

- DONE Tests. (Can always have more.)

- DONE Use async HTTP.

- DONE Support packfiles with deltas. (`ofs-delta` objects are still not
  supported.)

- DONE Optionally send authorization. Already possibly in the URL.

- DONE Better error messages.

- Better errors for non-existing user, repository, ref, PR, etc.

Optional improvements:

- Support `ofs-delta` objects in packfiles. Not necessarily, unless we
  send this capability, the server is not sending `ofs-delta` objects.

- Make unpacking faster. It is not fast currently, with all the bit
  arithmetic in R. But it is already faster than a `tar.gz` + uncompress
  download from GitHub, so not really needed.

### Docs and other helpful links:

- <https://github.com/git/git/blob/master/Documentation/gitprotocol-common.adoc>

- <https://github.com/git/git/blob/master/Documentation/gitprotocol-pack.adoc>

- <https://github.com/git/git/blob/master/Documentation/gitprotocol-v2.adoc>

- <https://github.com/calebsander/git-internals/blob/part2/src/main.rs>

- <https://dev.to/calebsander/git-internals-part-1-the-git-object-model-474m>
