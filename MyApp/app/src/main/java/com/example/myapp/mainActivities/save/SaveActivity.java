package com.example.myapp.mainActivities.save;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Build;
import android.os.Bundle;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.SeekBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.MainApplication;
import com.example.myapp.MusicPlayer;
import com.example.myapp.R;
import com.google.android.material.bottomnavigation.BottomNavigationView;

import java.util.ArrayList;

public class SaveActivity extends AppCompatActivity {

    MainApplication mainApplication;
    SaveViewModel saveViewModel;
    SaveListAdapter saveListAdapter;
    BottomNavigationView bottomNavigation;
    ListView listView;
    Button printButton;

    MusicPlayer musicPlayer;
    TextView songName;
    SeekBar songProgress;
    ImageButton songPrevious, songPause, songNext;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_save);
        mainApplication = (MainApplication) getApplication();
        saveViewModel = new ViewModelProvider(this).get(SaveViewModel.class);
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
        initialiseSaveLogs();
    }

    //link all components with ID
    public void initialiseViewByID(){
        musicPlayer = mainApplication.getMusicPlayer();
        bottomNavigation = findViewById(R.id.bottom_navigator);
        listView = findViewById(R.id.saveListView);
        songName = findViewById(R.id.songName);
        songProgress = findViewById(R.id.songProgress);
        songPrevious = findViewById(R.id.songPrevious);
        songPause = findViewById(R.id.songPause);
        songNext = findViewById(R.id.songNext);
        printButton = findViewById(R.id.printButton);
    }

    //initialise bottom navigator
    @SuppressLint("NonConstantResourceId")
    public void initialiseBottomNavigator(){
        //set current item to save icon
        bottomNavigation.setSelectedItemId(R.id.save);
        //set bottom navigator listener
        bottomNavigation.setOnItemSelectedListener(item -> {
            Intent intent = mainApplication.getIntent(item.getItemId(), R.id.save);
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

    public void initialiseSaveLogs(){
        saveListAdapter = new SaveListAdapter(this, R.layout.save_list_item, new ArrayList<>());
        listView.setAdapter(saveListAdapter);
        saveViewModel.getSaveLog().observeForever(saveLogs -> saveListAdapter.updateSaveLogs(saveLogs));
        printButton.setOnClickListener(v -> {
            if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.R)
                saveViewModel.copyLogFile();
            else
                saveViewModel.downloadLogFile(requestPermissionLauncher);
        });
    }

    private final ActivityResultLauncher<String> requestPermissionLauncher =
            registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
                if (isGranted) {
                    saveViewModel.copyLogFile();
                }
                else {
                    Toast.makeText(getApplicationContext(), "Permission not granted to print log files", Toast.LENGTH_SHORT).show();
                }
            });

    @Override
    protected void onResume() {
        super.onResume();
        //set bottom navigator to save icon on resume
        bottomNavigation.setSelectedItemId(R.id.save);
        //reset song name and progress if music player uninitialised
        musicPlayer.resetMusic(songName, songProgress);
    }
}