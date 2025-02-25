# NOTICE

**This project is not supported anymore.**

It's still here just to support legacy projects that may have it as a dependency.
It's operational (at least, for Erlang versions lower than OTP 21)

If you're looking for an HTTP client, we would recommend you to switch to
[shotgun](https://github.com/inaka/shotgun) or [fusco](https://github.com/esl/fusco)

## OTP 20.1 Warning

lhttpc has an issue with OTP 20.1 and a bug in the `server_name_indication` ssl option. See
[#12](https://github.com/erlcloud/lhttpc/pull/12) and [#13](https://github.com/erlcloud/lhttpc/pull/13)
for more details. For lhttpc users on OTP 20.x, it is recommended to upgrade to OTP 20.3 or greater
to avoid this potential issue.

## Dependencies:

 * Erlang/OTP R16B03-1 or newer
   * Application compiler to build, kernel, stdlib and ssl to run

## Building: 

For versions > 1.2.5, lhttpc is built using rebar. Take a look at 
http://bitbucket.org/basho/rebar/wiki/Home for more information. There is still
a Makefile with some of the old make targets, such as all, doc, test etc. for those who prefer
that. The makefile will however just call rebar.

## Configuration: (environment variables)

 * `connection_timeout`: The time (in milliseconds) the client will try to keep a HTTP/1.1
   connection open. Changing this value in runtime has no effect, this can however be done through
   `lhttpc_manager:update_connection_timeout/1`.
 * `pool_size`: The default number of client processes to allow in an `lhttpc_manager` pool.
   Can be overridden at pool creation time, or updated at run-time using
   `lhttpc_manager:set_max_pool_size/2`.