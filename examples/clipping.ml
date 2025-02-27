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

let pi = acos(-1.)

let () =

  Backend.(init default_options);

  let c = Canvas.createFramed "Clipping"
            ~pos:(300, 200) ~size:(300, 300) in
  Canvas.save c;
  Canvas.scale c (2.0, 2.0);
  Canvas.rotate c 0.3;

  let square = Path.create () in
  Path.rect square ~pos:(50., 50.) ~size:(50. ,50.);
  Canvas.clipPath c square ~nonzero:false;
  Canvas.setFillColor c Color.orange;
  Canvas.fillRect c ~pos:(0.,0.) ~size:(300., 300.);
  Canvas.show c;
  Canvas.restore c;
  Canvas.setFillColor c Color.red;
  Canvas.rect c ~pos:(150., 20.) ~size:(75., 75.);
  Canvas.scale c (2.0, 2.0);
  Canvas.rotate c 0.3;
  Canvas.rect c ~pos:(50., 50.) ~size:(50. ,50.);
  Canvas.clip c ~nonzero:false;
  Canvas.save c;
  Canvas.restore c;

  let p_dragonImage = ImageData.createFromPNG "assets/dragon.png" in
  ignore @@
    Promise.bind p_dragonImage (fun dragonImage ->
        let pattern = Canvas.createPattern c dragonImage RepeatXY in
        Canvas.setFillPattern c pattern;
        Canvas.scale c (0.1, 0.1);
        Canvas.rotate c (0. -. 0.3);
        Canvas.save c;
        Canvas.save c;
        Canvas.restore c;
        Canvas.restore c;
        Canvas.fillRect c ~pos:(0.,0.) ~size:(3000., 3000.);
        Promise.return ());

  Backend.run (fun state -> function

    | Event.KeyAction { canvas = _; timestamp = _;
                        key; char = _; flags = _; state = Down } ->
        if key = Event.KeyEscape then
          Backend.stop ();
        state, true

    | Event.CanvasClosed { canvas = _; timestamp = _ } ->
        Backend.stop ();
        state, true

    | Event.Frame { canvas = _; timestamp = _ } ->
        state, true

    | _ ->
        state, false

    ) (function _state ->
         Printf.printf "Goodbye !\n"
    ) ()
