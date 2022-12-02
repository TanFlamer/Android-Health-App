package com.example.myapp.mainActivities.save;

import android.annotation.SuppressLint;
import android.content.Intent;
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
import com.example.myapp.mainActivities.InfoActivity;
import com.example.myapp.mainActivities.MusicActivity;
import com.example.myapp.mainActivities.SleepActivity;
import com.example.myapp.mainActivities.SportActivity;
import com.google.android.material.bottomnavigation.BottomNavigationView;

import java.util.ArrayList;

public class SaveActivity extends AppCompatActivity {

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
        saveViewModel = new ViewModelProvider(this).get(SaveViewModel.class);
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseViewByID();
        initialiseBottomNavigator();
        initialiseMusicPlayer();
        initialiseSaveLogs();
    }

    public void initialiseViewByID(){
        musicPlayer = ((MainApplication) getApplication()).getMusicPlayer();
        bottomNavigation = findViewById(R.id.bottom_navigator);
        listView = findViewById(R.id.saveListView);
        songName = findViewById(R.id.songName);
        songProgress = findViewById(R.id.songProgress);
        songPrevious = findViewById(R.id.songPrevious);
        songPause = findViewById(R.id.songPause);
        songNext = findViewById(R.id.songNext);
        printButton = findViewById(R.id.printButton);
    }

    @SuppressLint("NonConstantResourceId")
    public void initialiseBottomNavigator(){
        bottomNavigation.setSelectedItemId(R.id.save);
        bottomNavigation.setOnItemSelectedListener(item -> {
            switch (item.getItemId()){
                case R.id.save:
                    return true;

                case R.id.sleep:
                    startActivity(new Intent(getApplicationContext(), SleepActivity.class));
                    return true;

                case R.id.music:
                    startActivity(new Intent(getApplicationContext(), MusicActivity.class));
                    return true;

                case R.id.sport:
                    startActivity(new Intent(getApplicationContext(), SportActivity.class));
                    return true;

                case R.id.info:
                    startActivity(new Intent(getApplicationContext(), InfoActivity.class));
                    return true;
            }
            return false;
        });
    }

    public void initialiseMusicPlayer(){
        initialiseSongController();
        initialiseImageButtons();
        initialiseLiveData();
    }

    public void initialiseSongController(){
        songProgress.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                if(fromUser) musicPlayer.setSongProgress(progress);
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {
                musicPlayer.pauseSong();
            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {
                musicPlayer.playSong();
            }
        });
    }

    public void initialiseImageButtons(){
        songPrevious.setOnClickListener(v -> musicPlayer.previousButton());
        songPause.setOnClickListener(v -> musicPlayer.playButton());
        songNext.setOnClickListener(v -> musicPlayer.nextButton());
    }

    public void initialiseLiveData(){
        musicPlayer.getSong().observeForever(song -> {
            songName.setText(song.getSongName());
            songProgress.setProgress(0);
            songProgress.setMax(song.getSongDuration() * 1000);
        });
        musicPlayer.getSongProgress().observeForever(integer -> songProgress.setProgress(integer));
    }

    public void initialiseSaveLogs(){
        saveListAdapter = new SaveListAdapter(this, R.layout.save_list_item, new ArrayList<>());
        listView.setAdapter(saveListAdapter);
        saveViewModel.getSaveLog().observeForever(saveLogs -> saveListAdapter.updateSaveLogs(saveLogs));
        printButton.setOnClickListener(v -> saveViewModel.downloadLogFile(requestPermissionLauncher));
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
        bottomNavigation.setSelectedItemId(R.id.save);
    }
}