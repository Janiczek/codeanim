# CodeAnim

Click to see demo:
[![Screenshot](https://github.com/Janiczek/codeanim/raw/master/doc/screenshot.png)](https://github.com/Janiczek/codeanim/raw/master/doc/screencast.mp4)

TODO:
* deploy to GitHub pages
* UI buttons to add/delete/reorder/change duration of actions
* Hold button -> repeatedly apply the action (step / fast forward)
* Scroll the timeline when jumping
* Audio track to play under the video?

## Recording with FFMPEG:

* `ffmpeg -video_size 1920x1080 -framerate 60 -f x11grab -i :0.0+0,0 no-settings.mp4` ~ [811 kB](https://github.com/Janiczek/codeanim/raw/master/doc/tests/no-settings.mp4)
* `ffmpeg -video_size 1920x1080 -framerate 60 -f x11grab -i :0.0+0,0 -c:v libx264 -preset ultrafast -crf 0 crf0-ultrafast.mp4` ~ [18 MB](https://github.com/Janiczek/codeanim/raw/master/doc/tests/crf0-ultrafast.mp4)
* `ffmpeg -i test2.mp4 -c:v libx264 -preset veryslow -crf 0 crf0-ultrafast-then-crf0-veryslow.mp4` ~ [3.5 MB](https://github.com/Janiczek/codeanim/raw/master/doc/tests/crf0-ultrafast-then-crf0-veryslow.mp4)
* `ffmpeg -video_size 1920x1080 -framerate 60 -f x11grab -i :0.0+0,0 -c:v libx264 -preset ultrafast -crf 17 crf17-veryfast.mp4` ~ [3.3 MB](https://github.com/Janiczek/codeanim/raw/master/doc/tests/crf17-ultrafast.mp4)

The first option is surprisingly readable, given it's 330 kb/s.
The third option (that is, first ultrafast CRF=0 then veryslow CRF=0 as a second
pass) looks like the best guarantee of real lossless output. And there seem to
be no frame drops.
