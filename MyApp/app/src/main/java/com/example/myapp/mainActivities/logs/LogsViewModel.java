package com.example.myapp.mainActivities.logs;

import android.Manifest;
import android.app.Application;
import android.content.pm.PackageManager;
import android.os.Environment;
import android.util.Pair;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.MutableLiveData;

import com.example.myapp.MainApplication;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.time.LocalDateTime;
import java.util.List;

public class LogsViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final String filePath;
    private final int userID;

    //constructor for logs view model
    public LogsViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        userID = mainApplication.getUserID();
        filePath = getApplication().getFilesDir().toString() + "/logs/" + userID + ".txt";
    }

    //copy logs file to download folder
    public void copyLogFile(){
        try{
            //get text file name
            String fileName = userID + ".txt";
            //get path to logs folder
            Path source = Paths.get(filePath);
            //get path to download folder
            Path dest = Paths.get(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS).toString(), fileName);
            //copy logs file to download folder, replacing any old file
            Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING);
            //show toast
            Toast.makeText(getApplication(), "Log file downloaded", Toast.LENGTH_SHORT).show();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    //check for permission to write external storage
    public void downloadLogFile(ActivityResultLauncher<String> requestPermissionLauncher){
        //if permission granted, copy logs file to download folder
        if (ContextCompat.checkSelfPermission(getApplication(), Manifest.permission.WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)
            copyLogFile();
        else //if permission denied, show toast
            requestPermissionLauncher.launch(Manifest.permission.WRITE_EXTERNAL_STORAGE);
    }

    //get save logs list from main application to show in list
    public MutableLiveData<List<Pair<String, LocalDateTime>>> getSaveLog() {
        return mainApplication.getSaveLog();
    }
}
