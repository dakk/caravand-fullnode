val fatal : string -> ('a, out_channel, unit) format -> 'a
val error : string -> ('a, out_channel, unit) format -> 'a
val warn  : string -> ('a, out_channel, unit) format -> 'a
val info  : string -> ('a, out_channel, unit) format -> 'a
val debug : string -> ('a, out_channel, unit) format -> 'a