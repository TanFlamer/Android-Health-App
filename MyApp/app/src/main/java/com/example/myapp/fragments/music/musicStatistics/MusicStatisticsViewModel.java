package com.example.myapp.fragments.music.musicStatistics;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;
import androidx.lifecycle.Transformations;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.song.Song;
import com.example.myapp.databasefiles.song.SongRepository;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogue;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogueRepository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class MusicStatisticsViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final SongRepository songRepository;
    private final SongCatalogueRepository songCatalogueRepository;

    private MediatorLiveData<double[]> musicDateMerger;
    private LiveData<List<Song>> songLiveData;
    private LiveData<List<SongCatalogue>> songCatalogueLiveData;

    private final int userID;

    //constructor for music statistics view model
    public MusicStatisticsViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        songRepository = mainApplication.getSongRepository();
        songCatalogueRepository = mainApplication.getSongCatalogueRepository();
        userID = mainApplication.getUserID();
        initialiseLiveData();
        initialiseLiveDataMerger();
    }

    //initialise live data for playlists and song list
    public void initialiseLiveData(){
        //initialise live data for song list
        songLiveData = songRepository.getAllSongs(userID);
        //initialise live data for song catalogue list
        songCatalogueLiveData = songCatalogueRepository.getAllSongCatalogue(userID);
    }

    //merge live data for song list and song catalogue list
    public void initialiseLiveDataMerger(){
        musicDateMerger = new MediatorLiveData<>();
        musicDateMerger.addSource(songLiveData, songs -> musicDateMerger.setValue(compilePlaylistResults(mainApplication.getSongList(), mainApplication.getSongCatalogueList())));
        musicDateMerger.addSource(songCatalogueLiveData, songCatalogues -> musicDateMerger.setValue(compilePlaylistResults(mainApplication.getSongList(), mainApplication.getSongCatalogueList())));
    }

    //compile song statistics
    public double[] compileSongResults(List<Song> songs){
        //if no songs return empty statistics
        if(songs.size() == 0) return new double[5];

        int[] results = new int[] {0, 0, Integer.MIN_VALUE, Integer.MAX_VALUE};
        for(Song song : songs){
            results[0] += song.getSongDuration(); //total song duration
            results[1] += 1; //total song count
            results[2] = Math.max(song.getSongDuration(), results[2]); //longest song
            results[3] = Math.min(song.getSongDuration(), results[3]); //shortest song
        }
        return new double[] { results[0], results[1], (double) results[0] / results[1], results[2], results[3] };
    }

    //link songs to playlists
    public double[] compilePlaylistResults(List<Song> songs, List<SongCatalogue> songCatalogues){
        //if no song catalogues return empty statistics
        if(songCatalogues.size() == 0) return new double[7];

        HashMap<Integer, Song> songHashMap = new HashMap<>();
        for(Song song : songs) songHashMap.put(song.getSongID(), song);

        HashMap<Integer, List<Song>> songCatalogueHashMap = new HashMap<>();
        for(SongCatalogue songCatalogue : songCatalogues){
            int playlistID = songCatalogue.getPlaylistID();
            Song song = songHashMap.get(songCatalogue.getSongID());
            songCatalogueHashMap.putIfAbsent(playlistID, new ArrayList<>());
            Objects.requireNonNull(songCatalogueHashMap.get(playlistID)).add(song);
        }
        //compile playlist statistics
        return processPlaylistResults(songCatalogueHashMap);
    }

    //compile playlist statistics
    public double[] processPlaylistResults(HashMap<Integer, List<Song>> songCatalogueHashMap){
        int[] results = new int[] {0, Integer.MIN_VALUE, Integer.MAX_VALUE, 0, Integer.MIN_VALUE, Integer.MAX_VALUE, 0};
        for(List<Song> songs : songCatalogueHashMap.values()){
            int songDuration = 0;
            int songCount = 0;
            for(Song song : songs){
                songCount++;
                songDuration += song.getSongDuration();
            }
            results[0] += 1; //playlist count
            results[1] = Math.max(songs.size(), results[1]); //most songs
            results[2] = Math.min(songs.size(), results[2]); //least songs
            results[3] += songDuration; //total song duration
            results[4] = Math.max(songDuration, results[4]); //longest playlist
            results[5] = Math.min(songDuration, results[5]); //shortest playlist
            results[6] += songCount; //total song count
        }
        return new double[] {results[0], (double) results[3] / results[0],
                (double) results[6] / results[0], results[6], results[5], results[1], results[2]};
    }

    //return live data for song list
    public LiveData<double[]> getSongLiveData() {
        return Transformations.map(songLiveData, this::compileSongResults);
    }

    //return live data merger of song list and song catalogue list
    public MediatorLiveData<double[]> getMusicDateMerger() {
        return musicDateMerger;
    }
}
