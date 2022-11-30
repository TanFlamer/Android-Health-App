package com.example.myapp.mainActivities.save;

import android.Manifest;
import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.Environment;
import android.util.Pair;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.SeekBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.content.ContextCompat;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.MainApplication;
import com.example.myapp.MusicPlayer;
import com.example.myapp.R;
import com.example.myapp.mainActivities.InfoActivity;
import com.example.myapp.mainActivities.MusicActivity;
import com.example.myapp.mainActivities.SleepActivity;
import com.example.myapp.mainActivities.SportActivity;
import com.google.android.material.bottomnavigation.BottomNavigationView;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
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
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.music:
                    startActivity(new Intent(getApplicationContext(), MusicActivity.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.sport:
                    startActivity(new Intent(getApplicationContext(), SportActivity.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.info:
                    startActivity(new Intent(getApplicationContext(), InfoActivity.class));
                    overridePendingTransition(0, 0);
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
        saveViewModel.getSaveLog().observeForever(stringLocalDateTimePair -> {
            saveListAdapter.updateSaveLogs(stringLocalDateTimePair);
            appendLogFile(stringLocalDateTimePair);
        });
        printButton.setOnClickListener(v -> downloadLogFile());
        separateLogFile();
    }

    public void separateLogFile(){
        try {
            String fileName = saveViewModel.getFilePath() + saveViewModel.getUserID() + ".txt";
            FileWriter fw = new FileWriter(fileName, true);
            fw.write("\n");
            fw.close();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void appendLogFile(Pair<String, LocalDateTime> stringLocalDateTimePair) {
        try {
            String fileName = saveViewModel.getFilePath() + saveViewModel.getUserID() + ".txt";
            FileWriter fw = new FileWriter(fileName, true);
            DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
            String formattedDateTime = stringLocalDateTimePair.second.format(dateTimeFormatter);
            fw.write(formattedDateTime + " " + stringLocalDateTimePair.first + "\n");
            fw.close();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void copyLogFile(){
        try{
            String fileName = saveViewModel.getUserID() + ".txt";
            Path source = Paths.get(saveViewModel.getFilePath(), fileName);
            Path dest = Paths.get(String.valueOf(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS)), fileName);
            Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING);
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        Toast.makeText(getApplicationContext(), "Log file downloaded", Toast.LENGTH_SHORT).show();
    }

    public void downloadLogFile(){
        if (ContextCompat.checkSelfPermission(getApplicationContext(), Manifest.permission.WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)
            copyLogFile();
        else
            requestPermissionLauncher.launch(Manifest.permission.WRITE_EXTERNAL_STORAGE);
    }

    private final ActivityResultLauncher<String> requestPermissionLauncher =
            registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
                if (isGranted) {
                    copyLogFile();
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