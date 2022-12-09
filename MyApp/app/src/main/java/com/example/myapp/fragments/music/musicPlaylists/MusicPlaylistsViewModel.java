package com.example.myapp.fragments.music.musicPlaylists;

import android.app.AlertDialog;
import android.app.Application;
import android.content.Context;
import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.MusicPlayer;
import com.example.myapp.databasefiles.playlist.Playlist;
import com.example.myapp.databasefiles.playlist.PlaylistRepository;
import com.example.myapp.databasefiles.song.Song;
import com.example.myapp.databasefiles.song.SongRepository;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogue;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogueRepository;
import com.example.myapp.subActivities.music.MusicDataActivity;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

public class MusicPlaylistsViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final PlaylistRepository playlistRepository;
    private final SongRepository songRepository;
    private final SongCatalogueRepository songCatalogueRepository;

    private MediatorLiveData<HashMap<Playlist, List<Song>>> musicDateMerger;
    private LiveData<List<Playlist>> playlistLiveData;
    private LiveData<List<Song>> songLiveData;
    private LiveData<List<SongCatalogue>> songCatalogueLiveData;

    private final MusicPlayer musicPlayer;
    private final int userID;

    //constructor for music playlists view model
    public MusicPlaylistsViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        playlistRepository = mainApplication.getPlaylistRepository();
        songRepository = mainApplication.getSongRepository();
        songCatalogueRepository = mainApplication.getSongCatalogueRepository();
        userID = mainApplication.getUserID();
        musicPlayer = mainApplication.getMusicPlayer();
        initialiseLiveData();
        initialiseLiveDataMerger();
    }

    //initialise live data of playlists, song list and song catalogue list
    public void initialiseLiveData(){
        playlistLiveData = playlistRepository.getAllPlaylists(userID);
        songLiveData = songRepository.getAllSongs(userID);
        songCatalogueLiveData = songCatalogueRepository.getAllSongCatalogue(userID);
    }

    //merge live data of playlists, song list and song catalogue list
    public void initialiseLiveDataMerger(){
        musicDateMerger = new MediatorLiveData<>();
        musicDateMerger.addSource(playlistLiveData, playlists -> musicDateMerger.setValue(processResults(mainApplication.getPlaylistList(), mainApplication.getSongList(), mainApplication.getSongCatalogueList())));
        musicDateMerger.addSource(songLiveData, songs -> musicDateMerger.setValue(processResults(mainApplication.getPlaylistList(), mainApplication.getSongList(), mainApplication.getSongCatalogueList())));
        musicDateMerger.addSource(songCatalogueLiveData, songCatalogues -> musicDateMerger.setValue(processResults(mainApplication.getPlaylistList(), mainApplication.getSongList(), mainApplication.getSongCatalogueList())));
    }

    //link songs to playlists
    public HashMap<Playlist, List<Song>> processResults(List<Playlist> playlists, List<Song> songs, List<SongCatalogue> songCatalogues){
        //if playlists, song list or song catalogue list empty, return empty map
        if(playlists.size() == 0 || songs.size() == 0 || songCatalogues.size() == 0) return new HashMap<>();

        //get all playlists
        HashMap<Integer, Playlist> playlistHashMap = new HashMap<>();
        for(Playlist playlist : playlists) playlistHashMap.put(playlist.getPlaylistID(), playlist);
        Set<Integer> playlistSet = new HashSet<>(playlistHashMap.keySet());

        //get all songs
        HashMap<Integer, Song> songHashMap = new HashMap<>();
        for(Song song : songs) songHashMap.put(song.getSongID(), song);

        //link songs to playlists
        HashMap<Playlist, List<Song>> songCatalogueHashMap = new HashMap<>();
        Set<Integer> catalogueSet = new HashSet<>();
        for(SongCatalogue songCatalogue : songCatalogues){
            Playlist playlist = playlistHashMap.get(songCatalogue.getPlaylistID());
            Song song = songHashMap.get(songCatalogue.getSongID());
            songCatalogueHashMap.putIfAbsent(playlist, new ArrayList<>());
            Objects.requireNonNull(songCatalogueHashMap.get(playlist)).add(song);
            catalogueSet.add(songCatalogue.getPlaylistID());
        }

        //delete playlist if no songs
        playlistSet.removeAll(catalogueSet);
        for(Integer playlistID : playlistSet) playlistRepository.delete(playlistHashMap.get(playlistID));

        //return hashmap of playlist and song list
        return songCatalogueHashMap;
    }

    //send playlist name to edit playlist activity
    public Intent editPlaylist(String playlistName){
        Intent intent = new Intent(getApplication(), MusicDataActivity.class);
        intent.putExtra("playlistName", playlistName);
        return intent;
    }

    //dialog to validate playlist deletion
    public AlertDialog deletePlaylist(Context context, Playlist playlist){
        return new AlertDialog.Builder(context)
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", (dialog, which) -> {
                    playlistRepository.delete(playlist);
                    updateSaveLogs("Playlist " + playlist.getPlaylistName() + " deleted");
                })
                .setNegativeButton("No", null)
                .create();
    }

    //sort playlists and song lists
    public void sortPlaylists(List<Playlist> playlists, HashMap<Playlist, List<Song>> songPlaylists, String data, String order){
        Comparator<Playlist> playlistComparator = getPlaylistComparator(data, order, songPlaylists);
        playlists.sort(playlistComparator);
        Comparator<Song> songComparator = getSongComparator(data, order);
        for(List<Song> songList : songPlaylists.values()) songList.sort(songComparator);
    }

    //get comparator to sort playlists
    public Comparator<Playlist> getPlaylistComparator(String data, String order, HashMap<Playlist, List<Song>> songPlaylists){
        Comparator<Playlist> playlistComparator = Comparator.comparingInt(Playlist::getPlaylistID);
        switch (data) {
            case "Date Added":
                playlistComparator = Comparator.comparingInt(Playlist::getPlaylistID);
                break;
            case "Name":
                playlistComparator = Comparator.comparing(Playlist::getPlaylistName);
                break;
            case "Length":
                playlistComparator = Comparator.comparing(playlist -> getPlaylistLength(playlist, songPlaylists));
                break;
        }
        return order.equals("Ascending") ? playlistComparator : playlistComparator.reversed();
    }

    //get comparator to sort song lists
    public Comparator<Song> getSongComparator(String data, String order){
        Comparator<Song> songComparator = Comparator.comparingInt(Song::getSongID);
        switch (data) {
            case "Date Added":
                songComparator = Comparator.comparingInt(Song::getSongID);
                break;
            case "Name":
                songComparator = Comparator.comparing(Song::getSongName);
                break;
            case "Length":
                songComparator = Comparator.comparing(Song::getSongDuration);
                break;
        }
        return order.equals("Ascending") ? songComparator : songComparator.reversed();
    }

    //get total length of songs in playlist
    public int getPlaylistLength(Playlist playlist, HashMap<Playlist, List<Song>> songPlaylists){
        int duration = 0;
        for(Song song : Objects.requireNonNull(songPlaylists.get(playlist)))
            duration += song.getSongDuration();
        return duration;
    }

    //return music player
    public MusicPlayer getMusicPlayer() {
        return musicPlayer;
    }

    //return live data merger of playlists, song list and song catalogue list
    public MediatorLiveData<HashMap<Playlist, List<Song>>> getMusicDateMerger() {
        return musicDateMerger;
    }

    //update any changes to logs
    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }
}
