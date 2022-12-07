package com.example.myapp;

import android.annotation.SuppressLint;
import android.content.Context;
import android.media.MediaPlayer;
import android.widget.ImageButton;
import android.widget.SeekBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.lifecycle.MutableLiveData;

import com.example.myapp.databasefiles.song.Song;

import java.io.IOException;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

public class MusicPlayer {

    private final MainApplication mainApplication;
    private final Context context;
    private final String filePath;
    private MediaPlayer mediaPlayer;
    private List<Song> playlist;

    //live data of current song
    private MutableLiveData<Song> currentSong;
    //live data of song progress
    private MutableLiveData<Integer> songProgress;

    //boolean to check if song is loaded
    private boolean initialised;
    //number of current track
    private int currentTrack;

    //initialise music player
    public MusicPlayer(MainApplication mainApplication, String fileDir){
        //initialise media player
        initialiseMediaPlayer();
        //initialise live data for current song and song progress
        initialiseLiveData();
        //start timer to check song progress
        updateSongProgress();
        //get main application
        this.mainApplication = mainApplication;
        //get context from application
        this.context = mainApplication.getApplicationContext();
        //get file path to music folder
        filePath = fileDir + "/music/";
        //set current track to 0
        currentTrack = 0;
    }

    //initialise live data for current song and song progress
    public void initialiseLiveData(){
        //initialise live data for current song
        currentSong = new MutableLiveData<>();
        //initialise live data for song progress
        songProgress = new MutableLiveData<>();
    }

    //initialise media player
    public void initialiseMediaPlayer(){
        //initialise media player
        mediaPlayer = new MediaPlayer();
        //play next song when song completes
        mediaPlayer.setOnCompletionListener(mp -> playSong(1));
        //start media player when song is ready
        mediaPlayer.setOnPreparedListener(mp -> {
            mediaPlayer.start();
            initialised = true;
        });
    }

    //set current playlist and song
    public void setPlaylist(List<Song> newPlaylist, int position){
        //reset media player if currently playing
        if(initialised) mediaPlayer.reset();
        //set new playlist
        playlist = newPlaylist;
        //set current track position
        currentTrack = position;
        //play current song
        playSong(0);
    }

    //choose which song to play
    public void playSong(int index){
        //get playlist size
        int playlistSize = playlist.size();
        //get new track number
        currentTrack = (((currentTrack + index) % playlistSize) + playlistSize) % playlistSize;
        //get new song
        Song song = playlist.get(currentTrack);
        //set current song to play
        currentSong.setValue(song);
        //get path to current song
        String musicPath = filePath + mainApplication.getUserID() + "/" + song.getSongName();
        //set media player to uninitialised
        initialised = false;
        //reset media player
        mediaPlayer.reset();
        try {
            //put new song in media player
            mediaPlayer.setDataSource(musicPath);
            //prepare new song to play
            mediaPlayer.prepareAsync();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    //play song if paused
    public void playSong(){
        if(initialised && !mediaPlayer.isPlaying())
            mediaPlayer.start();
    }

    //pause song if playing
    public void pauseSong(){
        if(initialised && mediaPlayer.isPlaying())
            mediaPlayer.pause();
    }

    //set play button functions
    public void playButton(){
        if(!initialised) //show toast if media player not initialised
            Toast.makeText(context, "No song loaded", Toast.LENGTH_SHORT).show();
        else if(mediaPlayer.isPlaying()) //pause song if playing
            mediaPlayer.pause();
        else //play song if paused
            mediaPlayer.start();
    }

    //set next button functions
    public void nextButton(){
        if(!initialised) //show toast if media player not initialised
            Toast.makeText(context, "No song loaded", Toast.LENGTH_SHORT).show();
        else //play next song
            playSong(1);
    }

    //set previous button functions
    public void previousButton(){
        if(!initialised) //show toast if media player not initialised
            Toast.makeText(context, "No song loaded", Toast.LENGTH_SHORT).show();
        else //play previous song
            playSong(-1);
    }

    //reset media player
    public void resetMediaPlayer(){
        //set media player to uninitialized
        initialised = false;
        //reset media player
        mediaPlayer.reset();
        //set playlist to null
        playlist = null;
    }

    //update song progress
    public void setSongProgress(int progress){
        //set song to current progress if initialised
        if(initialised) mediaPlayer.seekTo(progress);
    }

    //check and update song progress
    public void updateSongProgress(){
        //initialise new timer
        Timer timer = new Timer();
        //check and update song progress every 100 milliseconds
        timer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                if(initialised) songProgress.postValue(mediaPlayer.getCurrentPosition());
            }
        }, 0, 100);
    }

    //initialise song seek bar
    public void initialiseSongController(SeekBar seekBar){
        //initialise seek bar listener
        seekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override //change song progress if input from user
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                if(fromUser) setSongProgress(progress);
            }

            @Override //pause song when seek bar touched
            public void onStartTrackingTouch(SeekBar seekBar) {
                pauseSong();
            }

            @Override //play song when seek bar released
            public void onStopTrackingTouch(SeekBar seekBar) {
                playSong();
            }
        });
    }

    //initialise song buttons
    public void initialiseImageButtons(ImageButton songPrevious, ImageButton songPause, ImageButton songNext){
        //initialise previous button
        songPrevious.setOnClickListener(v -> previousButton());
        //initialise pause button
        songPause.setOnClickListener(v -> playButton());
        //initialise next button
        songNext.setOnClickListener(v -> nextButton());
    }

    //initialise song name and progress live data
    public void initialiseSongProgress(TextView songName, SeekBar seekBar){
        //reset song name and progress when new song loaded
        currentSong.observeForever(song -> {
            songName.setText(song.getSongName());
            seekBar.setProgress(0);
            seekBar.setMax(song.getSongDuration() * 1000);
        });
        //update song progress forever
        songProgress.observeForever(seekBar::setProgress);
    }

    //reset song name and progress
    @SuppressLint("SetTextI18n")
    public void resetMusic(TextView songName, SeekBar seekBar){
        if(!initialised) {
            songName.setText("No Song Loaded");
            seekBar.setProgress(0);
        }
    }
}
