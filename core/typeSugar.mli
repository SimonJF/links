module Check :
  sig
    open Sugartypes

    val program : Types.typing_environment
               -> (program * Types.tygroup_environment)
               -> program * Types.datatype * Types.typing_environment

    val sentence : Types.typing_environment
                -> (sentence * Types.tygroup_environment)
                -> sentence * Types.datatype * Types.typing_environment
  end
