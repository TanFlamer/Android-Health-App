package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.databaseFiles.entity.SongPlaylist;
import com.example.myapp.databaseFiles.repository.PlaylistRepository;
import com.example.myapp.databaseFiles.repository.SongPlaylistRepository;
import com.example.myapp.databaseFiles.repository.SongRepository;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DataMusicViewModel extends AndroidViewModel {

    private SongRepository songRepository;
    private PlaylistRepository playlistRepository;
    private SongPlaylistRepository songPlaylistRepository;

    private LiveData<List<Song>> allSongs;
    private MutableLiveData<List<Song>> selectedSongList;
    private MutableLiveData<List<Song>> unselectedSongList;

    private int userID;
    private int playListID;

    public DataMusicViewModel(@NonNull Application application) {
        super(application);
        songRepository = new SongRepository(application);
        playlistRepository = new PlaylistRepository(application);
        songPlaylistRepository = new SongPlaylistRepository(application);

        while (allSongs == null) allSongs = songRepository.getAllSongs(userID);
        System.out.println(Arrays.toString(allSongs.getValue().toArray()));

        selectedSongList = new MutableLiveData<>();
        selectedSongList.setValue(new ArrayList<>());
        unselectedSongList = new MutableLiveData<>();
        unselectedSongList.setValue(new ArrayList<>());

        userID = ((MainApplication) getApplication()).getUserID();
        playListID = 0;
    }

    public void setPlayListID(int playListID) {
        this.playListID = playListID;
    }

    public boolean validatePlaylistName(String playlistName){
        return playlistRepository.findPlaylist(playlistName).size() == 0;
    }

    public void populateLists(){
        if(playListID == 0)
            unselectedSongList.setValue(allSongs.getValue());
        else{
            List<SongPlaylist> songPlaylists = songPlaylistRepository.getSongPlaylist(playListID);
            List<Song> selectedList = new ArrayList<>();
            for(SongPlaylist songPlaylist : songPlaylists) selectedList.add(songRepository.getSong(songPlaylist.getSongID()).get(0));

            Set<Song> selectedSong = new HashSet<>(allSongs.getValue());
            for(Song song : selectedList) selectedSong.remove(song);
            List<Song> unselectedList = new ArrayList<>(selectedSong);

            selectedSongList.setValue(selectedList);
            unselectedSongList.setValue(unselectedList);
        }
    }

    public MutableLiveData<List<Song>> getSelectedSongs(){
        return selectedSongList;
    }

    public MutableLiveData<List<Song>> getUnselectedSongs(){
        return unselectedSongList;
    }
}
