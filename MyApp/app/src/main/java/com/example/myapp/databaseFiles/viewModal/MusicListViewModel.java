package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.databaseFiles.entity.User;
import com.example.myapp.databaseFiles.repository.SongRepository;

import java.util.List;

public class MusicListViewModel extends AndroidViewModel {

    private SongRepository songRepository;
    private LiveData<List<Song>> songList;
    private String filePath;
    private int userID;

    public MusicListViewModel(@NonNull Application application) {
        super(application);
        songRepository = new SongRepository(application);
        songList = songRepository.getAllSongs(userID);
        userID = loadUserID();
        filePath = getApplication().getFilesDir() + "/music/" + userID;
    }

    public int loadUserID(){
        MainApplication appState = (MainApplication) this.getApplication();
        return appState.getUserID();
    }

    public void insert(Song newSong){
        songRepository.insert(newSong);
    }

    public void update(Song newSong){
        songRepository.update(newSong);
    }

    public void delete(Song song){
        songRepository.delete(song);
    }

    public List<Song> findSong(int userID, String songName){
        return songRepository.findSong(userID, songName);
    }

    public LiveData<List<Song>> getSongList(){
        return songList;
    }

    public int getUserID() {
        return userID;
    }

    public void setUserID(int userID) {
        this.userID = userID;
    }

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }
}
