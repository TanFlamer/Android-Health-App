package com.example.myapp.fragments.music.musicPlaylists;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.MusicPlayer;
import com.example.myapp.databasefiles.playlist.Playlist;
import com.example.myapp.databasefiles.song.Song;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogue;
import com.example.myapp.databasefiles.playlist.PlaylistRepository;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogueRepository;
import com.example.myapp.databasefiles.song.SongRepository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class MusicPlaylistsViewModel extends AndroidViewModel {

    private PlaylistRepository playlistRepository;
    private SongRepository songRepository;
    private SongCatalogueRepository songCatalogueRepository;
    private LiveData<List<SongCatalogue>> songPlaylistList;
    private MusicPlayer musicPlayer;
    private int userID;

    private HashMap<Integer, Playlist> playlistList;
    private HashMap<Integer, Song> songList;

    public MusicPlaylistsViewModel(@NonNull Application application) {
        super(application);
        playlistRepository = new PlaylistRepository(application);
        songRepository = new SongRepository(application);
        songCatalogueRepository = new SongCatalogueRepository(application);
        userID = ((MainApplication) getApplication()).getUserID();
        songPlaylistList = songCatalogueRepository.getAllSongPlaylist(userID);
        playlistList = new HashMap<>();
        songList = new HashMap<>();
        musicPlayer = ((MainApplication) getApplication()).getMusicPlayer();
    }

    public void insert(SongCatalogue songCatalogue){
        songCatalogueRepository.insert(songCatalogue);
    }

    public void update(SongCatalogue songCatalogue){
        songCatalogueRepository.update(songCatalogue);
    }

    public void delete(SongCatalogue songCatalogue){
        songCatalogueRepository.delete(songCatalogue);
    }

    public void deletePlaylist(Playlist playlist){
        playlistRepository.delete(playlist);
    }

    public HashMap<Playlist, List<Song>> updateMusicPlaylists(List<SongCatalogue> songCatalogues){

        if(songCatalogues.size() == 0) return new HashMap<>();
        HashMap<Playlist, List<Song>> newSongPlaylist = new HashMap<>();

        for(SongCatalogue songCatalogue : songCatalogues){
            int playlistID = songCatalogue.getPlaylistID();
            int songID = songCatalogue.getSongID();

            Playlist playlist = playlistList.containsKey(playlistID) ? playlistList.get(playlistID) : playlistRepository.getPlaylist(playlistID);
            playlistList.putIfAbsent(playlistID, playlist);

            Song song = songList.containsKey(songID) ? songList.get(songID) : songRepository.getSong(songID);
            songList.putIfAbsent(songID, song);

            newSongPlaylist.putIfAbsent(playlist, new ArrayList<>());
            Objects.requireNonNull(newSongPlaylist.get(playlist)).add(song);
        }
        return newSongPlaylist;
    }

    public LiveData<List<SongCatalogue>> getSongPlaylistList() {
        return songPlaylistList;
    }

    public MusicPlayer getMusicPlayer() {
        return musicPlayer;
    }
}
