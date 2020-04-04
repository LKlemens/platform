// We need to import the CSS so that webpack will load it.
// The MiniCssExtractPlugin is used to separate it out into
// its own CSS file.
import css from "../css/app.css";

// webpack automatically bundles all modules in your
// entry points. Those entry points can be configured
// in "webpack.config.js".
//
// Import dependencies
//
import "phoenix_html";

// Import local files
//
// Local files can be imported directly using relative paths, for example:
// import socket from "./socket"
//
//

// Phoenix Socket
import { Socket } from "phoenix";
let socket = new Socket("/socket", {});
socket.connect();

// Elm
import { Elm } from "../elm/src/Main.elm";

const elmContainer = document.querySelector("#elm-container");
const platformer = document.querySelector("#platformer");

if (elmContainer) {
  Elm.Main.init({ node: elmContainer });
}

if (platformer) {
  let app = Elm.Games.Platformer.init({ node: platformer });

  let channel = socket.channel("score:platformer", {});

  channel
    .join()
    .receive("ok", (resp) => {
      console.log("Joined successfully", resp);
    })
    .receive("error", (resp) => {
      console.log("Unable to join", resp);
    });

  app.ports.broadcastScore.subscribe(function (scoreData) {
    console.log(
      `Broadcasting ${scoreData} score data from Elm using the broadcastScore port.`
    );
    channel.push("broadcast_score", { player_score: scoreData });
    // Later, we'll push the score data to the Phoenix channel
  });

  channel.on("broadcast_score", (payload) => {
    console.log(
      `Receiving ${payload.player_score} score data from Phoenix using the receivingScoreFromPhoenix port.`
    );
    app.ports.receiveScoreFromPhoenix.send({
      player_score: payload.player_score,
    });
  });
}
