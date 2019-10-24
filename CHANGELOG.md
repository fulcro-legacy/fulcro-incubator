# Change Log

0.0.39
------
- Removed errant specs

0.0.38
------
- Fixed route matching to maximal match

0.0.37
------
- Fixed bad spec in state machines

0.0.36
------
- Fixed bug in `dr/change-route-relative` when mounted non-router-target component has 
same ident as mounted router-target and added some validation/error reporting for invalid
paths.

0.0.35
------
- Fixed bug in pmutations when the return value is an error

0.0.34
---
- Fixed bug in pm targeting when the source is coming from a non-returning
value.

0.0.33
------
- Fixed bug in dynamic router that was causing deferred routes to fail
to complete by giving state machine time to transition.

0.0.32
------
- Changed state machines to track actor classes via Fulcro's new component
registry. This fixes inspect's ability to "snapshot" state that uses state 
machines.

0.0.31
------
- Fixed bug in pessimistic mutation return value handling

0.0.30
------
- Proxy computed props through dynamic routers

0.0.29
------
- UISM: Added back debug logging. Requires Fulcro 2.8.3 (which fixed a logging bug)
- Routing: Fixed SSR helper bug, added some debug logging, and namespaced the router IDs.
NOTE: If you were accessing the router states in app state, you will need to update your router IDs.
- Removed an annoying UISM warning about accessing non-started machines, since 
that can happen in render very easily.
- Routing: Added `target-ready!` for proper deferred resolution in mutations.

0.0.28
------
- UISM: Added support for alias/actor targeting to load 
- UISM: Added support for using actor name instead of class in load
- Dynamic Router: Added preliminary (probably quite alpha) support for SSR state.
- Dynamic Router: Added additional props for pending route rendering.

0.0.27
------
- Extended UISM functions.
- UISM now has minimal to-many support via ident helpers.

0.0.26
------
- Fix for SSR of dynamic router

0.0.25
------
- Fixed issue when reconciler doesn't exist

0.0.24
------
- Fixed a spec

0.0.23
------
- Support `::pm/mutation-return-key` on pessimistic mutations
- Fix `pessimistic-mutation` to send correct returning query to remote

0.0.22
------
- Added dynamic routing with docs

0.0.21
------
- Added the ability to trigger events on other state machines from within a handler.

0.0.20
------
- Added proper ident refresh list for remote mutations to ensure UI refresh
- Added apply-action and get-active-state

0.0.19
------
- Removed accidental use of timbre
- Dropped the use of Defn for function specs for the moment.

0.0.18
------
- Fixed a race condition on React lifecycles when using state machines that could lose loads.
- Added

0.0.17
------
- Added fallback support for loads

0.0.16
------
- Fixed duplicate source in JAR

0.0.15
------
- Finished state machine support for: remotes, aborting network
requests, state timeout events, and improved docs.
- Upgraded example to use timeouts, aborts, etc.

0.0.14
------
- Got rid of ghostwheel dependency madness
- Added CI support

0.0.13
------
- Added remote support to state machines

0.0.12
------
- Added UI State Machines.

0.0.11-1
--------
- Removed stray console.log 

0.0.11
------
- Added a `ptransact!` that is capable of composing pmutations with all other kinds.

0.0.10
------
- Added `update-io-progress!` for flicker free io progress, but able to distinguish
loads from mutations.

0.0.9
-----
- Added some helpers for flicker free progress updates and other state
display to pessimistic mutations. These combine load marker support.
- Added helpers for reading mutation errors and loading state for
the "current" component.

0.0.8
-----
- Made `pmutate!` honor declared refresh on the mutation
- pmutate! now adds loading status to target entity, if targeting
- `pmutate!` will not write a mutation response of "loading" with key into a
declared target, so the target can display progress if necessary.

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
