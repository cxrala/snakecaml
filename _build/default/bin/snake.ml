open! Base
open! Snake_lib

let () =
  Run.run ();
  Core_kernel.never_returns (Async.Scheduler.go ())
;;
