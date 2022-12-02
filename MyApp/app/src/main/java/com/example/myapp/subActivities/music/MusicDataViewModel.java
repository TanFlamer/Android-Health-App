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
import java.util.Objects;
import java.util.Set;

public class MusicDataViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final PlaylistRepository playlistRepository;
    private final SongCatalogueRepository songCatalogueRepository;

    private final HashMap<Integer, Song> songMap;
    private final List<Song> songList;
    private final List<SongCatalogue> songCatalogueList;

    private Playlist playlist;
    private final int userID;

    public MusicDataViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        playlistRepository = mainApplication.getPlaylistRepository();
        songCatalogueRepository = mainApplication.getSongCatalogueRepository();

        songCatalogueList = mainApplication.getSongCatalogueList();
        songList = mainApplication.getSongList();

        songMap = new HashMap<>();
        for(Song song : songList) songMap.put(song.getSongID(), song);

        userID = mainApplication.getUserID();
    }

    public String loadPlaylist(String playlistName){
        playlist = playlistName == null ? null : playlistRepository.findPlaylist(userID, playlistName);
        return playlistName == null ? "" : playlistName;
    }

    public boolean validatePlaylistName(String playlistName){
        return playlistRepository.findPlaylist(userID, playlistName) == null;
    }

    public void insertPlaylist(String newPlaylistName){
        updateSaveLogs("Playlist " + newPlaylistName + " added");
        int playListID = (int) playlistRepository.insert(new Playlist(newPlaylistName, userID));
        playlist = new Playlist(playListID, newPlaylistName, userID);
    }

    public void updatePlaylist(String newPlaylistName){
        updateSaveLogs("Playlist " + playlist.getPlaylistName() + " changed to " + newPlaylistName);
        playlist.setPlaylistName(newPlaylistName);
        playlistRepository.update(playlist);
    }

    public void insertSongPlaylist(int songID){
        String songName = Objects.requireNonNull(songMap.get(songID)).getSongName();
        updateSaveLogs("Song " + songName + " added to " + playlist.getPlaylistName());
        songCatalogueRepository.insert(new SongCatalogue(playlist.getPlaylistID(), songID, userID));
    }

    public void deleteSongPlaylist(int songID){
        String songName = Objects.requireNonNull(songMap.get(songID)).getSongName();
        updateSaveLogs("Song " + songName + " removed from " + playlist.getPlaylistName());
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

    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }
}
