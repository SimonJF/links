open CommonTypes

val compile :
    Value.env ->
    (int * int) option * Ir.computation ->
    (Value.database * Sql.query * Types.datatype) option

val compile_temporal_join :
    TableMode.t ->
    Value.env ->
    Ir.computation ->
    (Value.database * Sql.query * Types.datatype) option
