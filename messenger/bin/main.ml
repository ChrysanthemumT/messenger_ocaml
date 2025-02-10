open Lwt
open Lwt_io
open Unix

let listen_address = Unix.inet_addr_loopback
let backlog = 10

module Chat_utils = struct
  let write_message message oc = write_line oc message

  let acknowledge oc timestamp () =
    let encoded = Printf.sprintf "%s:message received" timestamp in
    write_message encoded oc

  let rec send_message oc =
   fun () ->
    Lwt.catch
      (fun () ->
        Lwt_io.read_line Lwt_io.stdin >>= fun msg ->
        let send_time = Unix.gettimeofday () in
        let encoded = Printf.sprintf "%f:%s" send_time msg in
        write_message encoded oc >>= send_message oc)
      (function
        | End_of_file -> Lwt.return_unit
        | exn ->
            Lwt_io.printf "Send error: %s\n" (Printexc.to_string exn)
            >>= fun () -> Lwt.return_unit)

  let rec receive_message ic oc =
    Lwt_io.read_line_opt ic >>= fun msg ->
    match msg with
    | Some msg -> (
        let colon_pos = String.index msg ':' in
        let timestamp = String.sub msg 0 colon_pos in
        let message =
          String.sub msg (colon_pos + 1) (String.length msg - colon_pos - 1)
        in
        match message with
        | "message received" ->
            let time_now = Unix.gettimeofday () in
            let time_sent = float_of_string timestamp in
            let rtt = time_now -. time_sent in
            Lwt_io.printf "%s rtt:%f\n" message rtt >>= fun () ->
            receive_message ic oc
        | _ ->
            Lwt_io.printf "Received: %s\n" message >>= fun () ->
            acknowledge oc timestamp () >>= fun () -> receive_message ic oc)
    | None -> Lwt_io.printf "Connection closed\n" >>= fun () -> Lwt.return_unit

  let handle_conn ic oc =
    Lwt.catch
      (fun () -> Lwt.join [ send_message oc (); receive_message ic oc ])
      (fun e ->
        Lwt_io.printf "Connection error: %s\n" (Printexc.to_string e)
        >>= fun () -> Lwt.return_unit)
end

module Server = struct
  let socket ~port =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    setsockopt sock SO_REUSEADDR true;
    (bind sock @@ ADDR_INET (listen_address, port) |> fun x -> ignore x);
    listen sock backlog;
    sock

  let accept_conn conn =
    let fd, addr = conn in
    let ic = of_fd ~mode:Input fd in
    let oc = of_fd ~mode:Output fd in
    let addr_str =
      match addr with
      | ADDR_UNIX path -> path
      | ADDR_INET (addr, port) ->
          Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port
    in
    Lwt_io.printf "New Client Connected: %s\n" addr_str >>= fun () ->
    Lwt.finalize
      (fun () -> Chat_utils.handle_conn ic oc)
      (fun () ->
        Lwt_io.printf "Closing connection to %s\n" addr_str >>= fun () ->
        Lwt_io.close ic >>= fun () -> Lwt_io.close oc)

  let start_server sock =
    let rec serve () =
      Lwt.catch
        (fun () -> Lwt_unix.accept sock >>= accept_conn >>= serve)
        (fun e ->
          Lwt_io.printf "Server error: %s\n" (Printexc.to_string e)
          >>= fun () -> serve ())
    in
    serve
end

module Client = struct
  let start_client server_addr port =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    setsockopt sock SO_REUSEADDR true;
    Lwt.catch
      (fun () ->
        connect sock (ADDR_INET (inet_addr_of_string server_addr, port))
        >>= fun () ->
        let oc = of_fd ~mode:Output sock in
        let ic = of_fd ~mode:Input sock in
        Lwt.finalize
          (fun () -> Chat_utils.handle_conn ic oc)
          (fun () ->
            Lwt_io.printf "Closing connection...\n" >>= fun () ->
            Lwt_io.close ic >>= fun () -> Lwt_io.close oc))
      (fun e ->
        Lwt_io.printf "Connection error: %s\n" (Printexc.to_string e)
        >>= fun () -> Lwt.return_unit)
end

let () =
  let mode = Sys.argv.(1) in
  Printf.printf "%s started\n%!" mode;
  match mode with
  | "server" ->
      let sock = Server.socket ~port:8000 in
      let server = Server.start_server sock in
      Lwt_main.run (server ())
  | "client" -> Lwt_main.run (Client.start_client Sys.argv.(2) 8000)
  | _ -> Printf.printf "invalid"
