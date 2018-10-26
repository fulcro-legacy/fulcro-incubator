# Change Log

0.0.8
-----
- Made `pmutate!` honor declared refresh on the mutation
- pmutate! now adds loading status to target entity, if targeting

0.0.7
-----
- Bugfix for pmutate!

0.0.6
-----
- Added support for mutation interfaces to be used with `pmutate!`

0.0.5
-----
- Renamed `::pm/error-marker` to `::pm/key`.
- Made `::pm/key` visible in loading and errors.

0.0.4
-----
- Fixed hard errors in pmutate to not call ok handler
- Added target/returning support to `pmutate!`
- Improved visibility rules rules for mutation response, and documented it.

0.0.3
-----
- Refined mutation interface declaration
- Added a new version of ptransact! in a new namespace. Much more refined.
  Requires Fulcro 2.6.9.

0.0.2
-----
- Added support for mutation interface declaration

0.0.1
-----
- Initial release
