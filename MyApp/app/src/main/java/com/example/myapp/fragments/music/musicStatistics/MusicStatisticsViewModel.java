package com.example.myapp.fragments.music.musicStatistics;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databasefiles.playlist.Playlist;
import com.example.myapp.databasefiles.playlist.PlaylistRepository;
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

    private MediatorLiveData<Pair<int[], int[]>> musicDateMerger;
    private LiveData<List<Song>> songLiveData;
    private LiveData<List<SongCatalogue>> songCatalogueLiveData;

    private final int userID;

    public MusicStatisticsViewModel(@NonNull Application application) {
        super(application);
        mainApplication = (MainApplication) getApplication();
        songRepository = mainApplication.getSongRepository();
        songCatalogueRepository = mainApplication.getSongCatalogueRepository();
        userID = mainApplication.getUserID();
        initialiseLiveData();
        initialiseLiveDataMerger();
    }

    public void initialiseLiveData(){
        songLiveData = songRepository.getAllSongs(userID);
        songCatalogueLiveData = songCatalogueRepository.getAllSongCatalogue(userID);
    }

    public void initialiseLiveDataMerger(){
        musicDateMerger = new MediatorLiveData<>();
        musicDateMerger.addSource(songLiveData, songs -> musicDateMerger.setValue(processResults(mainApplication.getSongList(), mainApplication.getSongCatalogueList())));
        musicDateMerger.addSource(songCatalogueLiveData, songCatalogues -> musicDateMerger.setValue(processResults(mainApplication.getSongList(), mainApplication.getSongCatalogueList())));
    }

    public Pair<int[], int[]> processResults(List<Song> songs, List<SongCatalogue> songCatalogues){
        if(songs.size() == 0 || songCatalogues.size() == 0) return new Pair<>(new int[4], new int[7]);

        HashMap<Integer, Song> songHashMap = new HashMap<>();
        for(Song song : songs) songHashMap.put(song.getSongID(), song);

        HashMap<Integer, List<Song>> songCatalogueHashMap = new HashMap<>();
        for(SongCatalogue songCatalogue : songCatalogues){
            int playlistID = songCatalogue.getPlaylistID();
            Song song = songHashMap.get(songCatalogue.getSongID());
            songCatalogueHashMap.putIfAbsent(playlistID, new ArrayList<>());
            Objects.requireNonNull(songCatalogueHashMap.get(playlistID)).add(song);
        }
        return new Pair<>(compileSongResults(songs), compilePlaylistResults(songCatalogueHashMap));
    }

    public int[] compileSongResults(List<Song> songList){
        int[] songResults = new int[] {0, 0, Integer.MIN_VALUE, Integer.MAX_VALUE};
        for(Song song : songList){
            songResults[0] += song.getSongDuration(); //total song duration
            songResults[1] += 1; //total song count
            songResults[2] = Math.max(song.getSongDuration(), songResults[2]); //longest song
            songResults[3] = Math.min(song.getSongDuration(), songResults[3]); //shortest song
        }
        return songResults;
    }

    public int[] compilePlaylistResults(HashMap<Integer, List<Song>> songCatalogueHashMap){
        int[] playlistResults = new int[] {0, Integer.MIN_VALUE, Integer.MAX_VALUE, 0, Integer.MIN_VALUE, Integer.MAX_VALUE, 0};
        for(List<Song> songs : songCatalogueHashMap.values()){
            int songDuration = 0;
            int songCount = 0;
            for(Song song : songs){
                songCount++;
                songDuration += song.getSongDuration();
            }
            playlistResults[0] += 1; //playlist count
            playlistResults[1] = Math.max(songs.size(), playlistResults[1]); //most songs
            playlistResults[2] = Math.min(songs.size(), playlistResults[2]); //least songs
            playlistResults[3] += songDuration; //total song duration
            playlistResults[4] = Math.max(songDuration, playlistResults[4]); //longest playlist
            playlistResults[5] = Math.min(songDuration, playlistResults[5]); //shortest playlist
            playlistResults[6] += songCount; //total song count
        }
        return playlistResults;
    }

    public MediatorLiveData<Pair<int[], int[]>> getMusicDateMerger() {
        return musicDateMerger;
    }
}
