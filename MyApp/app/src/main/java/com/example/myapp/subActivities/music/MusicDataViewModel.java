package com.example.myapp.subActivities.music;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.playlist.Playlist;
import com.example.myapp.databasefiles.playlist.PlaylistRepository;
import com.example.myapp.databasefiles.song.Song;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogue;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogueRepository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class MusicDataViewModel extends AndroidViewModel {

    private PlaylistRepository playlistRepository;
    private SongCatalogueRepository songCatalogueRepository;

    private HashMap<Integer, Song> songMap;
    private List<Song> songList;
    private List<SongCatalogue> songCatalogueList;

    private Playlist playlist;
    private int userID;

    public MusicDataViewModel(@NonNull Application application) {
        super(application);
        playlistRepository = ((MainApplication) getApplication()).getPlaylistRepository();
        songCatalogueRepository = ((MainApplication) getApplication()).getSongPlaylistRepository();

        songCatalogueList = ((MainApplication) getApplication()).getSongPlaylistList();
        songList = ((MainApplication) getApplication()).getSongList();

        songMap = new HashMap<>();
        for(Song song : songList) songMap.put(song.getSongID(), song);

        userID = ((MainApplication) getApplication()).getUserID();
    }

    public String loadPlaylist(String playlistName){
        playlist = playlistName == null ? null : playlistRepository.findPlaylist(userID, playlistName);
        return playlistName == null ? "" : playlistName;
    }

    public boolean validatePlaylistName(String playlistName){
        return playlistRepository.findPlaylist(userID, playlistName) == null;
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
        songCatalogueRepository.insert(new SongCatalogue(playlist.getPlaylistID(), songID, userID));
    }

    public void deleteSongPlaylist(int songID){
        songCatalogueRepository.delete(new SongCatalogue(playlist.getPlaylistID(), songID, userID));
    }

    public Pair<List<Song>, List<Song>> populateLists(){
        if(playlist == null)
            return new Pair<>(new ArrayList<>(songList), new ArrayList<>());
        else{
            List<Song> selectedList = new ArrayList<>();
            Set<Song> songSet = new HashSet<>(songList);

            List<SongCatalogue> songCatalogues = new ArrayList<>(songCatalogueList);
            songCatalogues.removeIf(songPlaylist -> !songPlaylist.getPlaylistID().equals(playlist.getPlaylistID()));

            for(SongCatalogue songCatalogue : songCatalogues){
                Song song = songMap.get(songCatalogue.getSongID());
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
