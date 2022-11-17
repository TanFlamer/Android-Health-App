package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.databaseFiles.repository.SongRepository;

import java.util.List;

public class SongViewModal extends AndroidViewModel {

    private SongRepository songRepository;
    private LiveData<List<Song>> allSongs;

    public SongViewModal(@NonNull Application application, int userID) {
        super(application);
        songRepository = new SongRepository(application);
        allSongs = songRepository.getAllSongs(userID);
    }

    public void insert(Song song) {
        songRepository.insert(song);
    }

    public void update(Song song) {
        songRepository.update(song);
    }

    public void delete(Song song) {
        songRepository.delete(song);
    }

    public List<Song> findSong(int songID){
        return songRepository.findSong(songID);
    }

    public LiveData<List<Song>> getAllSongs() {
        return allSongs;
    }
}
