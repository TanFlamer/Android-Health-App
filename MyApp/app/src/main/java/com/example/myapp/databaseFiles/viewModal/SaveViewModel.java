package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.MutableLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.repository.PlaylistRepository;
import com.example.myapp.databaseFiles.repository.SleepRepository;
import com.example.myapp.databaseFiles.repository.SongPlaylistRepository;
import com.example.myapp.databaseFiles.repository.SongRepository;
import com.example.myapp.databaseFiles.repository.SportRepository;
import com.example.myapp.databaseFiles.repository.TypeRepository;
import com.example.myapp.databaseFiles.repository.TypeSportRepository;

import java.time.LocalTime;
import java.util.List;

public class SaveViewModel extends AndroidViewModel {

    private SleepRepository sleepRepository;

    private SongRepository songRepository;
    private PlaylistRepository playlistRepository;
    private SongPlaylistRepository songPlaylistRepository;

    private SportRepository sportRepository;
    private TypeRepository typeRepository;
    private TypeSportRepository typeSportRepository;

    private int userID;

    public SaveViewModel(@NonNull Application application) {
        super(application);
        sleepRepository = new SleepRepository(application);
        songRepository = new SongRepository(application);
        playlistRepository = new PlaylistRepository(application);
        songPlaylistRepository = new SongPlaylistRepository(application);
        sportRepository = new SportRepository(application);
        typeRepository = new TypeRepository(application);
        typeSportRepository = new TypeSportRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public MutableLiveData<List<Pair<String, LocalTime>>> getSaveLogs() {
        return ((MainApplication) getApplication()).getSaveLogs();
    }
}
