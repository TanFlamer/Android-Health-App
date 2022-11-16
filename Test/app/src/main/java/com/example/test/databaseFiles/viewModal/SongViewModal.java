package com.example.test.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.test.databaseFiles.entity.Song;
import com.example.test.databaseFiles.repository.SongRepository;

import java.util.List;

public class SongViewModal extends AndroidViewModel {

    private SongRepository songRepository;
    private List<Song> allSongs;

    public SongViewModal(@NonNull Application application) {
        super(application);
        songRepository = new SongRepository(application);
        allSongs = songRepository.getAllSongs();
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

    public Song findSong(int songID){
        return songRepository.findSong(songID);
    }

    public List<Song> getAllSongs() {
        return allSongs;
    }
}
