(**************************************************************************)
(*                                                                        *)
(*    Copyright 2022 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OcamlCanvas.V1

let events = ref []

let retain_event e =
  events := e :: !events

let clear_events () =
  events := []

type state = {
  mutable frog_opt : Canvas.t option;
  mutable frames : Int64.t
}

let state = {
  frog_opt = None;
  frames = 0L;
}

let () =

  Backend.init ();

  let size_i = (400, 300) in
  let size_f = Point.of_ints size_i in
  let blue = Color.of_rgb 64 144 192 in
  let violet = Color.of_rgb 96 32 192 in
  let green = Color.of_rgb 32 192 96 in

  let c = Canvas.createOnscreen ~title:"Demo"
            ~pos:(500, 200) ~size:size_i () in

  Canvas.setFillColor c blue;
  Canvas.fillRect c ~pos:(0.0, 0.0) ~size:size_f;

  Canvas.setFont c "Liberation Sans" ~size:36.0
    ~slant:Font.Roman ~weight:Font.bold;
  Canvas.setFillColor c violet;
  Canvas.save c;
  Canvas.translate c (fst size_f /. 2.0, 50.0);
  Canvas.rotate c (-. Const.pi_8 *. 0.2);
  Canvas.fillText c "OCaml-Canvas" (-170.0, 0.0);
  Canvas.restore c;

  Canvas.setFillColor c green;
  Canvas.clearPath c;
  Canvas.moveTo c (20.0, 100.0);
  Canvas.lineTo c (280.0, 100.0);
  Canvas.lineTo c (280.0, 250.0);
  Canvas.lineTo c (20.0, 250.0);
  Canvas.closePath c;
  Canvas.fill c ~nonzero:false;

  Canvas.setGlobalAlpha c 0.7;
  Canvas.setFillColor c Color.red;
  Canvas.clearPath c;
  Canvas.moveTo c (40.0, 280.0);
  Canvas.lineTo c (90.0, 180.0);
  Canvas.lineTo c (140.0, 280.0);
  Canvas.closePath c;
  Canvas.fill c ~nonzero:false;

  Canvas.setFillColor c Color.orange;
  Canvas.clearPath c;
  Canvas.moveTo c (200.0, 230.0);
  Canvas.arc c ~center:(200.0, 230.0) ~radius:50.0
    ~theta1:0.0 ~theta2:(2.0 *. Const.pi) ~ccw:false;
  Canvas.closePath c;
  Canvas.fill c ~nonzero:false;

  Canvas.setStrokeColor c Color.blue;
  Canvas.setLineWidth c 5.0;
  Canvas.clearPath c;
  Canvas.moveTo c (40.0, 230.0);
  Canvas.bezierCurveTo c ~cp1:(80.0, 40.0)
    ~cp2:(220.0, 310.0) ~p:(260.0, 120.0);
  Canvas.stroke c;

  Canvas.setGlobalAlpha c 1.0;

  Canvas.show c;

  let event_frog = Canvas.createOffscreenFromPNG "assets/frog.png" in
  retain_event @@
    React.E.map (fun frog ->
        state.frog_opt <- Some (frog);
        let size = Canvas.getSize frog in
        Canvas.save c;
        Canvas.setTransform c Transform.id;
        Canvas.scale c (0.25, 0.25);
        Canvas.blit ~dst:c ~dpos:(1150, 700) ~src:frog ~spos:(0, 0) ~size;
        Canvas.restore c
      ) event_frog;

  retain_event @@
    React.E.map (fun { Event.canvas = _; timestamp = _; data = () } ->
        Backend.stop ()
      ) Event.close;

  retain_event @@
    React.E.map (fun { Event.canvas = _; timestamp = _;
                       data = { Event.key; char = _; flags = _ }; _ } ->
        if key = KeyEscape then
          Backend.stop ()
      ) Event.key_down;

  retain_event @@
    React.E.map (fun { Event.canvas = c; timestamp = _;
                       data = { Event.position = (x, y); button } } ->
        match button, state.frog_opt with
        | ButtonRight, Some (frog) ->
            let size = Canvas.getSize frog in
            let w, h = size in
            Canvas.save c;
            Canvas.setTransform c Transform.id;
            Canvas.translate c (float_of_int x, float_of_int y);
            Canvas.scale c (0.25, 0.25);
            Canvas.translate c (-0.5 *. float_of_int w, -0.5 *. float_of_int h);
            Canvas.blit ~dst:c ~dpos:(0, 0) ~src:frog ~spos:(0, 0) ~size;
            Canvas.restore c
        | ButtonLeft, _ ->
            Canvas.setFillColor c Color.red;
            Canvas.clearPath c;
            Canvas.arc c ~center:(float_of_int x, float_of_int y)
              ~radius:5.0 ~theta1:0.0 ~theta2:(2.0 *. Const.pi) ~ccw:false;
            Canvas.fill c ~nonzero:false;
        | _ ->
            ()
      ) Event.button_down;

  retain_event @@
    React.E.map (fun { Event.canvas = _; timestamp = _; data = _ } ->
        state.frames <- Int64.add state.frames Int64.one
      ) Event.frame;

  Backend.run (fun () ->
      clear_events ();
      Printf.printf "Displayed %Ld frames. Goodbye !\n" state.frames)
