# CodeAnim

Click to see demo:
[![Screenshot](https://github.com/Janiczek/codeanim/raw/master/doc/screenshot.png)](https://github.com/Janiczek/codeanim/raw/master/doc/screencast.mp4)

TODO:
* deploy to GitHub pages
* UI buttons to add/delete/reorder/change duration of actions
* ~Export to SVG/GIF/MP4/PNGs/...~
  * With the current way we render, neither `html2canvas` nor `dom-to-image*` are able to capture the DOM element properly. Looks like either we'll have to render it differently (hardcoded width/height for the preview / full-scene div and no SVG?)
  * Or maybe have a Canvas-based renderer which would take serialized Scenes?
  * Likely easier solution than that: play in fullscreen mode and just capture that with OBS?
* Hold button -> repeatedly apply the action (step / fast forward)
* Scroll the timeline when jumping
* Audio track to play under the video?
