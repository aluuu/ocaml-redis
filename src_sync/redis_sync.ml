module IO = struct
  type 'a t = 'a

  type flow = {
    fd: Unix.file_descr;
    in_channel: Pervasives.in_channel;
    out_channel: Pervasives.out_channel
  }

  type socket_domain = Unix.socket_domain
  type socket_type = Unix.socket_type
  type socket_addr = Unix.sockaddr

  type 'a stream = 'a Stream.t
  type stream_count = int

  let (>>=) a f = f a
  let catch f exn_handler = try f () with e -> exn_handler e
  let try_bind f bind_handler exn_handler = try f () >>= bind_handler with e -> exn_handler e
  let ignore_result = ignore
  let return a = a
  let fail e = raise e
  let run a = a

  let connect host port =
    let port = string_of_int port in
    let addr_info =
      let open Unix in
      match getaddrinfo host port [AI_FAMILY PF_INET] with
      | ai::_ -> ai
      | [] ->
          match getaddrinfo host port [AI_FAMILY PF_INET6] with
          | ai::_ -> ai
          | []    -> failwith "Could not resolve redis host!"
    in
    let fd = Unix.socket addr_info.Unix.ai_family Unix.SOCK_STREAM 0 in
    try
      Unix.connect fd addr_info.Unix.ai_addr;
      let in_channel = Unix.in_channel_of_descr fd in
      let out_channel = Unix.out_channel_of_descr fd in
      { fd = fd; in_channel = in_channel; out_channel = out_channel }
    with
      exn -> Unix.close fd; raise exn

  let close flow = Unix.close flow.fd
  let sleep a = ignore (Unix.select [] [] [] a)

  let input_char flow = Pervasives.input_char flow.in_channel
  let really_input flow = Pervasives.really_input flow.in_channel
  let output_string flow = output_string flow.out_channel
  let flush flow = Pervasives.flush flow.out_channel

  let iter = List.iter
  let iter_serial = List.iter
  let map = List.map
  let map_serial = List.map
  let fold_left = List.fold_left

  let stream_from = Stream.from
  let stream_next = Stream.next
end

module Client = Redis.Client.Make(IO)
module Cache = Redis.Cache.Make(IO)(Client)
module Mutex = Redis.Mutex.Make(IO)(Client)
