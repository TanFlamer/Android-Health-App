package com.example.myapp.mainActivities.save;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.MutableLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.playlist.PlaylistRepository;
import com.example.myapp.databaseFiles.sleep.SleepRepository;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylistRepository;
import com.example.myapp.databaseFiles.song.SongRepository;
import com.example.myapp.databaseFiles.sport.SportRepository;
import com.example.myapp.databaseFiles.type.TypeRepository;
import com.example.myapp.databaseFiles.typeSport.TypeSportRepository;

import java.time.LocalTime;
import java.util.List;

public class SaveViewModel extends AndroidViewModel {

    private String filePath;
    private int userID;

    public SaveViewModel(@NonNull Application application) {
        super(application);
        userID = ((MainApplication) getApplication()).getUserID();
        filePath = getApplication().getFilesDir().toString() + "/logs/";
    }

    public MutableLiveData<Pair<String, LocalTime>> getSaveLog() {
        return ((MainApplication) getApplication()).getSaveLog();
    }

    public String getFilePath() {
        return filePath;
    }

    public int getUserID() {
        return userID;
    }
}
