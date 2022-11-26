package com.example.myapp.fragments.music.musicStatistics;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MediatorLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.playlist.PlaylistRepository;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.databaseFiles.song.SongRepository;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylist;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylistRepository;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.typeSport.TypeSport;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class MusicStatisticsViewModel extends AndroidViewModel {

    private SongRepository songRepository;
    private SongPlaylistRepository songPlaylistRepository;

    private MediatorLiveData<Pair<int[], int[]>> songDateMerger;
    private LiveData<List<Song>> songLiveData;
    private LiveData<List<SongPlaylist>> songPlaylistLiveData;

    private int userID;

    public MusicStatisticsViewModel(@NonNull Application application) {
        super(application);
        songRepository = ((MainApplication) getApplication()).getSongRepository();
        songPlaylistRepository = ((MainApplication) getApplication()).getSongPlaylistRepository();
        userID = ((MainApplication) getApplication()).getUserID();
        initialiseLists();
        initialiseLiveDataMerger();
    }

    public void initialiseLists(){
        songLiveData = songRepository.getAllSongs(userID);
        songPlaylistLiveData = songPlaylistRepository.getAllSongPlaylist(userID);
    }

    public void initialiseLiveDataMerger(){
        songDateMerger = new MediatorLiveData<>();
        songDateMerger.addSource(songLiveData, typeList -> songDateMerger.setValue(processResults(((MainApplication) getApplication()).getSongList(), ((MainApplication) getApplication()).getSongPlaylistList())));
        songDateMerger.addSource(songPlaylistLiveData, typeSportList -> songDateMerger.setValue(processResults(((MainApplication) getApplication()).getSongList(), ((MainApplication) getApplication()).getSongPlaylistList())));
    }

    public Pair<int[], int[]> processResults(List<Song> songList, List<SongPlaylist> songPlaylistList){
        int[] songResults = new int[] {0, 0, Integer.MIN_VALUE, Integer.MAX_VALUE};
        HashMap<Integer, Song> songHashMap = new HashMap<>();
        for(Song song : songList){
            songHashMap.put(song.getSongID(), song);
            songResults[0] += song.getSongDuration(); //total song duration
            songResults[1] += 1; //total song count
            songResults[2] = Math.max(song.getSongDuration(), songResults[2]); //longest song
            songResults[3] = Math.min(song.getSongDuration(), songResults[3]); //shortest song
        }

        HashMap<Integer, List<Song>> playlistHashMap = new HashMap<>();
        for(SongPlaylist songPlaylist : songPlaylistList){
            int playlistID = songPlaylist.getPlaylistID();
            int songID = songPlaylist.getSongID();
            playlistHashMap.putIfAbsent(playlistID, new ArrayList<>());
            Objects.requireNonNull(playlistHashMap.get(playlistID)).add(songHashMap.get(songID));
        }

        int[] playlistResults = new int[] {0, Integer.MIN_VALUE, Integer.MAX_VALUE, 0, Integer.MIN_VALUE, Integer.MAX_VALUE, 0};
        for(List<Song> songs : playlistHashMap.values()){
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
        return new Pair<>(songResults, playlistResults);
    }

    public MediatorLiveData<Pair<int[], int[]>> getSongDateMerger() {
        return songDateMerger;
    }
}
