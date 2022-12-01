package com.example.myapp;

import android.content.Context;
import android.media.MediaPlayer;
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

    private MutableLiveData<Song> currentSong;
    private MutableLiveData<Integer> songProgress;

    private boolean initialised;
    private int currentTrack;

    public MusicPlayer(MainApplication mainApplication, String fileDir){
        initialiseMediaPlayer();
        initialiseLiveData();
        updateSongProgress();
        this.mainApplication = mainApplication;
        this.context = mainApplication.getApplicationContext();
        filePath = fileDir + "/music/";
        currentTrack = 0;
    }

    public void initialiseLiveData(){
        currentSong = new MutableLiveData<>();
        songProgress = new MutableLiveData<>();
    }

    public void initialiseMediaPlayer(){
        mediaPlayer = new MediaPlayer();
        mediaPlayer.setOnCompletionListener(mp -> playSong(1));
        mediaPlayer.setOnPreparedListener(mp -> {
            mediaPlayer.start();
            initialised = true;
        });
    }

    public void setPlaylist(List<Song> newPlaylist, int position){
        if(initialised) mediaPlayer.reset();
        playlist = newPlaylist;
        currentTrack = position;
        playSong(0);
    }

    public void playSong(int index){
        int playlistSize = playlist.size();
        currentTrack = (((currentTrack + index) % playlistSize) + playlistSize) % playlistSize;
        Song song = playlist.get(currentTrack);
        currentSong.setValue(song);
        String musicPath = filePath + mainApplication.getUserID() + "/" + song.getSongName();
        initialised = false;
        mediaPlayer.reset();
        try {
            mediaPlayer.setDataSource(musicPath);
            mediaPlayer.prepareAsync();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void playSong(){
        if(initialised && !mediaPlayer.isPlaying())
            mediaPlayer.start();
    }

    public void pauseSong(){
        if(initialised && mediaPlayer.isPlaying())
            mediaPlayer.pause();
    }

    public void playButton(){
        if(!initialised)
            Toast.makeText(context, "No song loaded", Toast.LENGTH_SHORT).show();
        else if(mediaPlayer.isPlaying())
            mediaPlayer.pause();
        else
            mediaPlayer.start();
    }

    public void nextButton(){
        if(!initialised)
            Toast.makeText(context, "No song loaded", Toast.LENGTH_SHORT).show();
        else
            playSong(1);
    }

    public void previousButton(){
        if(!initialised)
            Toast.makeText(context, "No song loaded", Toast.LENGTH_SHORT).show();
        else
            playSong(-1);
    }

    public void resetMediaPlayer(){
        initialised = false;
        mediaPlayer.reset();
        playlist = null;
    }

    public void setSongProgress(int progress){
        if(initialised) mediaPlayer.seekTo(progress);
        System.out.println("lol");
    }

    public void updateSongProgress(){
        Timer timer = new Timer();
        timer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                if(initialised) songProgress.postValue(mediaPlayer.getCurrentPosition());
            }
        }, 0, 100);
    }

    public boolean isInitialised() {
        return initialised;
    }

    public MutableLiveData<Song> getSong() {
        return currentSong;
    }

    public MutableLiveData<Integer> getSongProgress() {
        return songProgress;
    }
}
