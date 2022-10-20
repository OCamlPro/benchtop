include module type of Lwt.Infix
include module type of Lwt.Syntax

val (>>?) :
  ('a, 'b) Lwt_result.t ->
  ('a -> ('c, 'b) Lwt_result.t) ->
  ('c, 'b) Lwt_result.t

val (>|?) :
  ('a, 'b) Lwt_result.t ->
  ('a -> 'c) ->
  ('c, 'b) Lwt_result.t

val (>>!) :
  ('a, 'b) Lwt_result.t ->
  ('b -> ('a, 'c) Lwt_result.t) ->
  ('a, 'c) Lwt_result.t

val (>|!) :
  ('a, 'b) Lwt_result.t ->
  ('b -> 'c) ->
  ('a, 'c) Lwt_result.t 

val (let*?) :
  ('a, 'b) Lwt_result.t ->
  ('a -> ('c, 'b) Lwt_result.t) ->
  ('c, 'b) Lwt_result.t

val (and*?) :
  ('a, 'b) Lwt_result.t ->
  ('c, 'b) Lwt_result.t -> 
  (('a * 'c), 'b) Lwt_result.t

val (let+?) :
  ('a, 'b) Lwt_result.t ->
  ('a -> 'c) ->
  ('c, 'b) Lwt_result.t

val (and+?) :
  ('a, 'b) Lwt_result.t -> 
  ('c, 'b) Lwt_result.t -> 
  (('a * 'c), 'b) Lwt_result.t
