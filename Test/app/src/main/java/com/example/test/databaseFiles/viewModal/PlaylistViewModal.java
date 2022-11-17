package com.example.test.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.test.databaseFiles.entity.Playlist;
import com.example.test.databaseFiles.repository.PlaylistRepository;

import java.util.List;

public class PlaylistViewModal extends AndroidViewModel {

    private PlaylistRepository playlistRepository;
    private LiveData<List<Playlist>> allPlaylists;

    public PlaylistViewModal(@NonNull Application application, int userID) {
        super(application);
        playlistRepository = new PlaylistRepository(application);
        allPlaylists = playlistRepository.getAllPlaylists(userID);
    }

    public void insert(Playlist playlist) {
        playlistRepository.insert(playlist);
    }

    public void update(Playlist playlist) {
        playlistRepository.update(playlist);
    }

    public void delete(Playlist playlist) {
        playlistRepository.delete(playlist);
    }

    public List<Playlist> findPlaylist(int playlistID){
        return playlistRepository.findPlaylist(playlistID);
    }

    public LiveData<List<Playlist>> getAllPlaylists() {
        return allPlaylists;
    }
}
