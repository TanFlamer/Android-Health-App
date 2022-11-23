package com.example.myapp;

import android.app.Application;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;

import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.databaseFiles.entity.Type;
import com.example.myapp.databaseFiles.repository.SongRepository;
import com.example.myapp.databaseFiles.repository.TypeRepository;
import com.example.myapp.databaseFiles.repository.TypeSportRepository;
import com.example.myapp.databaseFiles.repository.UserRepository;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

public class MainApplication extends Application {

    private MutableLiveData<List<Pair<String, LocalTime>>> saveLogs;
    private int userID;

    private UserRepository userRepository;
    private SongRepository songRepository;
    private TypeRepository typeRepository;
    private TypeSportRepository typeSportRepository;

    private List<Song> songList;
    private List<Type> typeList;

    @Override
    public void onCreate() {
        super.onCreate();
        saveLogs = new MutableLiveData<>();
        saveLogs.setValue(new ArrayList<>());
        songList = new ArrayList<>();
        typeList = new ArrayList<>();
    }

    public void updateSaveLogs(Pair<String, LocalTime> newSaveLog){
        List<Pair<String, LocalTime>> oldSaveLogs = saveLogs.getValue();
        oldSaveLogs.add(newSaveLog);
        saveLogs.setValue(oldSaveLogs);
    }

    public UserRepository getUserRepository() {
        if(userRepository == null)
            userRepository = new UserRepository(this);
        return userRepository;
    }

    public SongRepository getSongRepository(){
        if(songRepository == null) {
            songRepository = new SongRepository(this);
            songRepository.getAllSongs(userID).observeForever(newSongList -> songList = newSongList);
        }
        return songRepository;
    }

    public TypeRepository getTypeRepository(){
        if(typeRepository == null) {
            typeRepository = new TypeRepository(this);
            typeRepository.getAllTypes(userID).observeForever(newTypeList -> typeList = newTypeList);
        }
        return typeRepository;
    }

    public TypeSportRepository getTypeSportRepository(){
        if(typeSportRepository == null)
            typeSportRepository = new TypeSportRepository(this);
        return typeSportRepository;
    }

    public MutableLiveData<List<Pair<String, LocalTime>>> getSaveLogs() {
        return saveLogs;
    }

    public int getUserID() {
        return userID;
    }

    public void setUserID(int userID) {
        this.userID = userID;
    }

    public List<Song> getSongList() {
        return songList;
    }

    public List<Type> getTypeList() {
        return typeList;
    }
}
