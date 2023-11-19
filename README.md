## Group members

* Yinzhen Xu (xuyinzhen@ucsd.edu)
* Helin Xu (TODO)
* Yifei Chen (yic107@ucsd.edu)
* Yuan Ning (yuning@ucsd.edu)

## Project Proposal

### Overview

Our project features a command-line gray-scale media player written in Haskell. Images or video frames are rendered in ASCII characters. It supports basic functionalities such as pausing, seeking, playback control, and forward/backward navigation. It also allows users to adjust brightness, contrast, and gamma of the video.

Besides, our media player incorporates text-based features. Subtitles can be loaded from a srt file and displayed on the screen. We will even introduce danmaku, a popular feature that enables viewers to post comments right on the screen to communicate with others.

In an era of increasingly complex multimedia players, our project serves as a lightweight, user-friendly alternative, particularly suitable for resource-constrained environments or situations requiring quick setup. By transforming media into ASCII art, we aim to offer a unique, minimalist viewing experience that is both resource-efficient and creatively stimulating.

### Implementation Details

#### Video

We will be using [TODO] to load videos. For RGB videos, we will manually convert them to gray-scale. Due to the limitation of terminal, we will not support audio.

#### Interaction

##### Command-line

Users start the program by running a command-line program with customized arguments.

This will include:

```
--video: the path to the video file
--srt: the path to the srt file
--danmaku: the path to the danmaku file
--brightness: the brightness of the video
--contrast: the contrast of the video
--gamma: the gamma of the video
--speed: the speed of the video
```

##### Real-time

For real-time interactions, users will be able to control the player via keyboard and mouse. The player will respond to the following signals:
1. Mouse click: pause, seek, forward/backward;
2. Arrow keys: forward/backward;
3. `:` + key: adjust brightness, contrast, gamma, speed;

#### Subtitles

Subtitle files are in srt format. We will parse the file and display the subtitles with a distinct color and background.

#### Danmaku

Danmaku files are in a srt-like format. Danmaku will be rolling from right to left on the screen.
