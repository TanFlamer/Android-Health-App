package com.example.myapp.mainActivities.save;

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

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class SaveViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final String filePath;
    private final int userID;

    public SaveViewModel(@NonNull Application application) {
        super(application);
        mainApplication = (MainApplication) getApplication();
        userID = mainApplication.getUserID();
        filePath = getApplication().getFilesDir().toString() + "/logs/" + userID + ".txt";
    }

    public void separateLogFile(){
        try {
            FileWriter fw = new FileWriter(filePath, true);
            fw.write("\n");
            fw.close();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void appendLogFile(Pair<String, LocalDateTime> stringLocalDateTimePair) {
        try {
            FileWriter fw = new FileWriter(filePath, true);
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
            String fileName = userID + ".txt";
            Path source = Paths.get(filePath);
            Path dest = Paths.get(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS).toString(), fileName);
            Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING);
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        Toast.makeText(getApplication(), "Log file downloaded", Toast.LENGTH_SHORT).show();
    }

    public void downloadLogFile(ActivityResultLauncher<String> requestPermissionLauncher){
        if (ContextCompat.checkSelfPermission(getApplication(), Manifest.permission.WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)
            copyLogFile();
        else
            requestPermissionLauncher.launch(Manifest.permission.WRITE_EXTERNAL_STORAGE);
    }

    public MutableLiveData<Pair<String, LocalDateTime>> getSaveLog() {
        return mainApplication.getSaveLog();
    }
}
