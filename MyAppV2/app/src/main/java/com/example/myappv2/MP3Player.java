package com.example.myappv2;

import android.content.Context;
import android.media.MediaPlayer;
import android.net.Uri;
import android.widget.ImageButton;
import android.widget.TextView;

public class MP3Player {

    public MediaPlayer mediaPlayer;
    public TextView songName;
    public ImageButton previous, pause, next;

    public MP3Player(Context context){
        Uri song = Uri.parse("android.resource://com.example.myappv2/raw/sample");
        mediaPlayer = MediaPlayer.create(context, song);
    }

    public void play(){
        mediaPlayer.start();
    }
}
