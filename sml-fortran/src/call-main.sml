(* Invoke main() for MLton *)
val _ = OS.Process.exit (Driver.main (CommandLine.name (),
                                      CommandLine.arguments ()))
