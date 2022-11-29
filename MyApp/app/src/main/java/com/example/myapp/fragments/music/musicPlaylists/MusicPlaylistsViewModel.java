package com.example.myapp.fragments.music.musicPlaylists;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.MusicPlayer;
import com.example.myapp.databaseFiles.playlist.Playlist;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylist;
import com.example.myapp.databaseFiles.playlist.PlaylistRepository;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylistRepository;
import com.example.myapp.databaseFiles.song.SongRepository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class MusicPlaylistsViewModel extends AndroidViewModel {

    private PlaylistRepository playlistRepository;
    private SongRepository songRepository;
    private SongPlaylistRepository songPlaylistRepository;
    private LiveData<List<SongPlaylist>> songPlaylistList;
    private MusicPlayer musicPlayer;
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
        musicPlayer = ((MainApplication) getApplication()).getMusicPlayer();
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

    public void deletePlaylist(Playlist playlist){
        playlistRepository.delete(playlist);
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

    public MusicPlayer getMusicPlayer() {
        return musicPlayer;
    }
}
