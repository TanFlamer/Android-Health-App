package com.example.myapp.mainActivities.save;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.MutableLiveData;

import com.example.myapp.MainApplication;

import java.time.LocalDateTime;

public class SaveViewModel extends AndroidViewModel {

    private String filePath;
    private int userID;

    public SaveViewModel(@NonNull Application application) {
        super(application);
        userID = ((MainApplication) getApplication()).getUserID();
        filePath = getApplication().getFilesDir().toString() + "/logs/";
    }

    public MutableLiveData<Pair<String, LocalDateTime>> getSaveLog() {
        return ((MainApplication) getApplication()).getSaveLog();
    }

    public String getFilePath() {
        return filePath;
    }

    public int getUserID() {
        return userID;
    }
}
