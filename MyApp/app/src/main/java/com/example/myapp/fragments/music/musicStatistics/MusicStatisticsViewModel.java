package com.example.myapp.fragments.music.musicStatistics;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.playlist.PlaylistRepository;
import com.example.myapp.databaseFiles.song.SongRepository;

public class MusicStatisticsViewModel extends AndroidViewModel {

    private PlaylistRepository playlistRepository;
    private SongRepository songRepository;
    private int userID;

    public MusicStatisticsViewModel(@NonNull Application application) {
        super(application);
        playlistRepository = new PlaylistRepository(application);
        songRepository = new SongRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
    }
}
