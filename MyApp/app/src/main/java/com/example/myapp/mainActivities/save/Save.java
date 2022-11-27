package com.example.myapp.mainActivities.save;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.content.ContextCompat;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;

import android.Manifest;
import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.Environment;
import android.util.Pair;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;
import android.widget.Toast;

import com.example.myapp.R;
import com.example.myapp.mainActivities.info.Info;
import com.example.myapp.mainActivities.music.Music;
import com.example.myapp.mainActivities.sleep.Sleep;
import com.example.myapp.mainActivities.sport.Sport;
import com.google.android.material.bottomnavigation.BottomNavigationView;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.time.LocalTime;
import java.util.ArrayList;

public class Save extends AppCompatActivity {

    SaveViewModel saveViewModel;
    BottomNavigationView bottomNavigation;
    ListView listView;
    Button printButton;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_save);
        saveViewModel = new ViewModelProvider(this).get(SaveViewModel.class);
        initialiseBottomNavigator();
        initialiseListView();
        initialiseButton();
    }

    public void initialiseButton(){
        printButton = findViewById(R.id.printButton);
        printButton.setOnClickListener(v -> downloadLogFile());
    }

    public void initialiseListView(){
        listView = findViewById(R.id.saveListView);
        SaveListAdapter saveListAdapter = new SaveListAdapter(this, R.layout.save_list_item, new ArrayList<>());
        listView.setAdapter(saveListAdapter);
        separateLogFile();
        saveViewModel.getSaveLog().observeForever(stringLocalTimePair -> {
            saveListAdapter.updateSaveLogs(stringLocalTimePair);
            appendLogFile(stringLocalTimePair);
        });
    }

    @SuppressLint("NonConstantResourceId")
    public void initialiseBottomNavigator(){
        bottomNavigation = findViewById(R.id.bottom_navigator);
        bottomNavigation.setSelectedItemId(R.id.save);
        bottomNavigation.setOnItemSelectedListener(item -> {
            switch (item.getItemId()){
                case R.id.save:
                    return true;

                case R.id.sleep:
                    startActivity(new Intent(getApplicationContext(), Sleep.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.music:
                    startActivity(new Intent(getApplicationContext(), Music.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.sport:
                    startActivity(new Intent(getApplicationContext(), Sport.class));
                    overridePendingTransition(0, 0);
                    return true;

                case R.id.info:
                    startActivity(new Intent(getApplicationContext(), Info.class));
                    overridePendingTransition(0, 0);
                    return true;
            }
            return false;
        });
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

    public void appendLogFile(Pair<String, LocalTime> stringLocalTimePair) {
        try {
            String fileName = saveViewModel.getFilePath() + saveViewModel.getUserID() + ".txt";
            FileWriter fw = new FileWriter(fileName, true);
            fw.write(stringLocalTimePair.second + " " + stringLocalTimePair.first + "\n");
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