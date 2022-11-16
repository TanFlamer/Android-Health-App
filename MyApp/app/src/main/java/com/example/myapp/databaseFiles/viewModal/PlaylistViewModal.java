package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.databaseFiles.entity.Playlist;
import com.example.myapp.databaseFiles.repository.PlaylistRepository;

import java.util.List;

public class PlaylistViewModal extends AndroidViewModel {

    private PlaylistRepository playlistRepository;
    private List<Playlist> allPlaylists;

    public PlaylistViewModal(@NonNull Application application) {
        super(application);
        playlistRepository = new PlaylistRepository(application);
        allPlaylists = playlistRepository.getAllPlaylists();
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

    public Playlist findPlaylist(int playlistID){
        return playlistRepository.findPlaylist(playlistID);
    }

    public List<Playlist> getAllPlaylists() {
        return allPlaylists;
    }
}
