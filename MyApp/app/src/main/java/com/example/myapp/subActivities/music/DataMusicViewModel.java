package com.example.myapp.subActivities.music;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.playlist.Playlist;
import com.example.myapp.databaseFiles.playlist.PlaylistRepository;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylist;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylistRepository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DataMusicViewModel extends AndroidViewModel {

    private PlaylistRepository playlistRepository;
    private SongPlaylistRepository songPlaylistRepository;

    private HashMap<Integer, Song> songMap;
    private List<Song> songList;
    private List<SongPlaylist> songPlaylistList;

    private Playlist playlist;
    private int userID;

    public DataMusicViewModel(@NonNull Application application) {
        super(application);
        playlistRepository = ((MainApplication) getApplication()).getPlaylistRepository();
        songPlaylistRepository = ((MainApplication) getApplication()).getSongPlaylistRepository();

        songPlaylistList = ((MainApplication) getApplication()).getSongPlaylistList();
        songList = ((MainApplication) getApplication()).getSongList();

        songMap = new HashMap<>();
        for(Song song : songList) songMap.put(song.getSongID(), song);

        userID = ((MainApplication) getApplication()).getUserID();
    }

    public String loadPlaylist(String playlistName){
        playlist = playlistName == null ? null : playlistRepository.findPlaylist(userID, playlistName).get(0);
        return playlistName == null ? "" : playlistName;
    }

    public boolean validatePlaylistName(String playlistName){
        return playlistRepository.findPlaylist(userID, playlistName).size() == 0;
    }

    public void insertPlaylist(String newPlaylistName){
        int playListID = (int) playlistRepository.insert(new Playlist(newPlaylistName, userID));
        playlist = new Playlist(playListID, newPlaylistName, userID);
    }

    public void updatePlaylist(String newPlaylistName){
        playlist.setPlaylistName(newPlaylistName);
        playlistRepository.update(playlist);
    }

    public void deletePlaylist(){
        playlistRepository.delete(playlist);
        playlist = null;
    }

    public void insertSongPlaylist(int songID){
        songPlaylistRepository.insert(new SongPlaylist(playlist.getPlaylistID(), songID, userID));
    }

    public void deleteSongPlaylist(int songID){
        songPlaylistRepository.delete(new SongPlaylist(playlist.getPlaylistID(), songID, userID));
    }

    public Pair<List<Song>, List<Song>> populateLists(){
        if(playlist == null)
            return new Pair<>(new ArrayList<>(songList), new ArrayList<>());
        else{
            List<Song> selectedList = new ArrayList<>();
            Set<Song> songSet = new HashSet<>(songList);

            List<SongPlaylist> songPlaylists = new ArrayList<>(songPlaylistList);
            songPlaylists.removeIf(songPlaylist -> !songPlaylist.getPlaylistID().equals(playlist.getPlaylistID()));

            for(SongPlaylist songPlaylist : songPlaylists){
                Song song = songMap.get(songPlaylist.getSongID());
                selectedList.add(song);
                songSet.remove(song);
            }
            return new Pair<>(new ArrayList<>(songSet), selectedList);
        }
    }

    public Playlist getPlaylist() {
        return playlist;
    }
}
