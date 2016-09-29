module IO = struct
  type 'a t = 'a Lwt.t

  type flow = {
    flow: Conduit_lwt_unix.flow;
    in_channel : Conduit_lwt_unix.ic;
    out_channel : Conduit_lwt_unix.oc;
  }

  type 'a stream = 'a Lwt_stream.t
  type stream_count = unit

  let (>>=) = Lwt.(>>=)
  let catch = Lwt.catch
  let try_bind = Lwt.try_bind
  let ignore_result = Lwt.ignore_result
  let return = Lwt.return
  let fail = Lwt.fail
  let run = Lwt_main.run


  let connect host port =
    let uri = (Uri.make ~scheme:"redis" ~host ~port) () in

    let service name =
      let open Resolver in
      return (Some {name=name; port=port; tls=false}) in
    let rewrites = ["", Resolver_lwt_unix.system_resolver] in
    let res = Resolver_lwt.init ~service ~rewrites () in

    Resolver_lwt.resolve_uri ~uri res >>= fun endp ->
    let ctx = Conduit_lwt_unix.default_ctx in
    Conduit_lwt_unix.endp_to_client ~ctx endp >>= fun client ->
    Conduit_lwt_unix.connect ~ctx client >>= fun (flow, ic, oc) ->
    return {flow=flow; in_channel=ic; out_channel=oc}

  let close flow = Lwt_io.close flow.in_channel
  let sleep = Lwt_unix.sleep

  let input_char {in_channel} = Lwt_io.read_char in_channel
  let really_input {in_channel} = Lwt_io.read_into_exactly in_channel
  let output_string {out_channel} = Lwt_io.write out_channel
  let flush {out_channel} = Lwt_io.flush out_channel

  let iter = Lwt_list.iter_p
  let iter_serial = Lwt_list.iter_s
  let map = Lwt_list.map_p
  let map_serial = Lwt_list.map_s
  let fold_left = Lwt_list.fold_left_s

  let stream_from = Lwt_stream.from
  let stream_next = Lwt_stream.next
end

module Client = Redis.Client.Make(IO)
module Cache = Redis.Cache.Make(IO)(Client)
module Mutex = Redis.Mutex.Make(IO)(Client)
