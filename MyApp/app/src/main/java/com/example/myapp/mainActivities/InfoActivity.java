package com.example.myapp.mainActivities;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Bundle;
import android.widget.ImageButton;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;

import com.example.myapp.MainApplication;
import com.example.myapp.MusicPlayer;
import com.example.myapp.R;
import com.google.android.material.bottomnavigation.BottomNavigationView;

public class InfoActivity extends AppCompatActivity {

    MainApplication mainApplication;
    BottomNavigationView bottomNavigation;

    MusicPlayer musicPlayer;
    TextView songName;
    SeekBar songProgress;
    ImageButton songPrevious, songPause, songNext;

    @SuppressLint("NonConstantResourceId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_info);
        mainApplication = (MainApplication) getApplication();
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //link all components with ID
        initialiseViewByID();
        //initialise bottom navigator
        initialiseBottomNavigator();
        //initialise music player
        initialiseMusicPlayer();
    }

    //link all components with ID
    public void initialiseViewByID(){
        musicPlayer = mainApplication.getMusicPlayer();
        bottomNavigation = findViewById(R.id.bottom_navigator);
        songName = findViewById(R.id.songName);
        songProgress = findViewById(R.id.songProgress);
        songPrevious = findViewById(R.id.songPrevious);
        songPause = findViewById(R.id.songPause);
        songNext = findViewById(R.id.songNext);
    }

    //initialise bottom navigator
    @SuppressLint("NonConstantResourceId")
    public void initialiseBottomNavigator(){
        //set current item to info icon
        bottomNavigation.setSelectedItemId(R.id.info);
        //set bottom navigator listener
        bottomNavigation.setOnItemSelectedListener(item -> {
            Intent intent = mainApplication.getIntent(item.getItemId(), R.id.info);
            if(intent != null) startActivity(intent);
            return true;
        });
    }

    //initialise music player
    public void initialiseMusicPlayer(){
        //initialise song seek bar
        musicPlayer.initialiseSongController(songProgress);
        //initialise song buttons
        musicPlayer.initialiseImageButtons(songPrevious, songPause, songNext);
        //initialise song name and progress live data
        musicPlayer.initialiseSongProgress(songName, songProgress);
    }

    @Override
    protected void onResume() {
        super.onResume();
        //set bottom navigator to info icon on resume
        bottomNavigation.setSelectedItemId(R.id.info);
        //reset song name and progress if music player uninitialised
        musicPlayer.resetMusic(songName, songProgress);
    }
}