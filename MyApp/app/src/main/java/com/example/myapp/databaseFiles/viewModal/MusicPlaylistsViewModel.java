package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.Playlist;
import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.databaseFiles.entity.SongPlaylist;
import com.example.myapp.databaseFiles.repository.PlaylistRepository;
import com.example.myapp.databaseFiles.repository.SongPlaylistRepository;
import com.example.myapp.databaseFiles.repository.SongRepository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class MusicPlaylistsViewModel extends AndroidViewModel {

    private PlaylistRepository playlistRepository;
    private SongRepository songRepository;
    private SongPlaylistRepository songPlaylistRepository;
    private LiveData<List<SongPlaylist>> songPlaylistList;
    private int userID;

    private HashMap<Integer, Playlist> playlistList;
    private HashMap<Integer, Song> songList;

    public MusicPlaylistsViewModel(@NonNull Application application) {
        super(application);
        playlistRepository = new PlaylistRepository(application);
        songRepository = new SongRepository(application);
        songPlaylistRepository = new SongPlaylistRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
        songPlaylistList = songPlaylistRepository.getAllSongPlaylist(userID);
        playlistList = new HashMap<>();
        songList = new HashMap<>();
    }

    public void insert(SongPlaylist songPlaylist){
        songPlaylistRepository.insert(songPlaylist);
    }

    public void update(SongPlaylist songPlaylist){
        songPlaylistRepository.update(songPlaylist);
    }

    public void delete(SongPlaylist songPlaylist){
        songPlaylistRepository.delete(songPlaylist);
    }

    public List<SongPlaylist> findSongPlaylist(int playlistID, int songID){
        return songPlaylistRepository.findSongPlaylist(playlistID, songID);
    }

    public HashMap<Playlist, List<Song>> updateMusicPlaylists(List<SongPlaylist> songPlaylists){

        if(songPlaylists.size() == 0) return new HashMap<>();
        HashMap<Playlist, List<Song>> newSongPlaylist = new HashMap<>();

        for(SongPlaylist songPlaylist : songPlaylists){
            int playlistID = songPlaylist.getPlaylistID();
            int songID = songPlaylist.getSongID();

            Playlist playlist = playlistList.containsKey(playlistID) ? playlistList.get(playlistID) : playlistRepository.getPlaylist(playlistID).get(0);
            playlistList.putIfAbsent(playlistID, playlist);

            Song song = songList.containsKey(songID) ? songList.get(songID) : songRepository.getSong(songID).get(0);
            songList.putIfAbsent(songID, song);

            newSongPlaylist.putIfAbsent(playlist, new ArrayList<>());
            Objects.requireNonNull(newSongPlaylist.get(playlist)).add(song);
        }
        return newSongPlaylist;
    }

    public LiveData<List<SongPlaylist>> getSongPlaylistList() {
        return songPlaylistList;
    }
}
