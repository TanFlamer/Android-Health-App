package com.example.myapp.fragments.music.musicPlaylists;

import android.app.AlertDialog;
import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.MusicPlayer;
import com.example.myapp.databasefiles.playlist.Playlist;
import com.example.myapp.databasefiles.song.Song;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogue;
import com.example.myapp.databasefiles.playlist.PlaylistRepository;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogueRepository;
import com.example.myapp.databasefiles.song.SongRepository;
import com.example.myapp.databasefiles.sport.Sport;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.subActivities.music.MusicDataActivity;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

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

    public MusicPlaylistsViewModel(@NonNull Application application) {
        super(application);
        mainApplication = (MainApplication) getApplication();
        playlistRepository = mainApplication.getPlaylistRepository();
        songRepository = mainApplication.getSongRepository();
        songCatalogueRepository = mainApplication.getSongCatalogueRepository();
        userID = mainApplication.getUserID();
        musicPlayer = mainApplication.getMusicPlayer();
        initialiseLiveData();
        initialiseLiveDataMerger();
    }

    public void initialiseLiveData(){
        playlistLiveData = playlistRepository.getAllPlaylists(userID);
        songLiveData = songRepository.getAllSongs(userID);
        songCatalogueLiveData = songCatalogueRepository.getAllSongCatalogue(userID);
    }

    public void initialiseLiveDataMerger(){
        musicDateMerger = new MediatorLiveData<>();
        musicDateMerger.addSource(playlistLiveData, playlists -> musicDateMerger.setValue(processResults(mainApplication.getPlaylistList(), mainApplication.getSongList(), mainApplication.getSongCatalogueList())));
        musicDateMerger.addSource(songLiveData, songs -> musicDateMerger.setValue(processResults(mainApplication.getPlaylistList(), mainApplication.getSongList(), mainApplication.getSongCatalogueList())));
        musicDateMerger.addSource(songCatalogueLiveData, songCatalogues -> musicDateMerger.setValue(processResults(mainApplication.getPlaylistList(), mainApplication.getSongList(), mainApplication.getSongCatalogueList())));
    }

    public HashMap<Playlist, List<Song>> processResults(List<Playlist> playlists, List<Song> songs, List<SongCatalogue> songCatalogues){
        if(playlists.size() == 0 || songs.size() == 0 || songCatalogues.size() == 0) return new HashMap<>();

        HashMap<Integer, Playlist> playlistHashMap = new HashMap<>();
        for(Playlist playlist : playlists) playlistHashMap.put(playlist.getPlaylistID(), playlist);

        HashMap<Integer, Song> songHashMap = new HashMap<>();
        for(Song song : songs) songHashMap.put(song.getSongID(), song);

        HashMap<Playlist, List<Song>> songCatalogueHashMap = new HashMap<>();
        for(SongCatalogue songCatalogue : songCatalogues){
            Playlist playlist = playlistHashMap.get(songCatalogue.getPlaylistID());
            Song song = songHashMap.get(songCatalogue.getSongID());
            songCatalogueHashMap.putIfAbsent(playlist, new ArrayList<>());
            Objects.requireNonNull(songCatalogueHashMap.get(playlist)).add(song);
        }
        return songCatalogueHashMap;
    }

    public MusicPlayer getMusicPlayer() {
        return musicPlayer;
    }

    public MediatorLiveData<HashMap<Playlist, List<Song>>> getMusicDateMerger() {
        return musicDateMerger;
    }

    public Intent editPlaylist(String playlistName){
        Intent intent = new Intent(getApplication(), MusicDataActivity.class);
        intent.putExtra("playlistName", playlistName);
        return intent;
    }

    public AlertDialog deletePlaylist(Context context, Playlist playlist){
        return new AlertDialog.Builder(context)
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", (dialog, which) -> playlistRepository.delete(playlist))
                .setNegativeButton("No", null)
                .create();
    }

    /*public Comparator<Playlist> getPlaylistComparator(String data, String order){
        Comparator<Playlist> playlistComparator = Comparator.comparingInt(Playlist::getPlaylistID);
        switch (data) {
            case "Date Added":
                playlistComparator = Comparator.comparingInt(Playlist::getPlaylistID);
                break;
            case "Name":
                playlistComparator = Comparator.comparing(Playlist::getPlaylistName);
                break;
            case "Length":
                playlistComparator = Comparator.comparing(this::getPlaylistLength);
                break;
        }
        return order.equals("Ascending") ? playlistComparator : playlistComparator.reversed();
    }

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

    public int getPlaylistLength(Playlist playlist){
        int duration = 0;
        for(Song song : Objects.requireNonNull(songPlaylists.get(playlist)))
            duration += song.getSongDuration();
        return duration;
    }*/
}
