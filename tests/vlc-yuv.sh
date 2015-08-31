# run qv4l2 raw yuvu frames on vlc
# https://github.com/koradlow/v4l2-rds-ctl/tree/master/utils/qv4l2
# https://wiki.videolan.org/YUV/
vlc --rawvid-fps 125 --rawvid-width 320 --rawvid-height 240 --rawvid-chroma YUVU /home/rafael/teste/rawframes.yuv
