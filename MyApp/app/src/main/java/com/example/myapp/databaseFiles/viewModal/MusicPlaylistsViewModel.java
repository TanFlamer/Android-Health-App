package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.Playlist;
import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.databaseFiles.entity.SongPlaylist;
import com.example.myapp.databaseFiles.repository.PlaylistRepository;
import com.example.myapp.databaseFiles.repository.SongPlaylistRepository;
import com.example.myapp.databaseFiles.repository.SongRepository;
import com.example.myapp.fragmentsMusic.expandableListMusic.MusicExpandableListData;
import com.example.myapp.fragmentsMusic.expandableListMusic.MusicExpandableListItem;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class MusicPlaylistsViewModel extends AndroidViewModel {

    private PlaylistRepository playlistRepository;
    private SongRepository songRepository;
    private SongPlaylistRepository songPlaylistRepository;
    private LiveData<List<SongPlaylist>> songPlaylistList;
    private int userID;

    public MusicPlaylistsViewModel(@NonNull Application application) {
        super(application);
        playlistRepository = new PlaylistRepository(application);
        songRepository = new SongRepository(application);
        songPlaylistRepository = new SongPlaylistRepository(application);
        userID = loadUserID();
        songPlaylistList = songPlaylistRepository.getAllSongPlaylist(userID);
    }

    public int loadUserID(){
        MainApplication appState = this.getApplication();
        return appState.getUserID();
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

    public List<SongPlaylist> findSongPlaylist(int playlistID, int songID){
        return songPlaylistRepository.findSongPlaylist(playlistID, songID);
    }

    public List<MusicExpandableListItem> updateMusicPlaylists(){
        if(songPlaylistList.getValue() == null) return new ArrayList<>();
        HashMap<Integer, List<Integer>> playlistMap = new HashMap<>();
        for(SongPlaylist songPlaylist : songPlaylistList.getValue()){
            int playlistID = songPlaylist.getPlaylistID();
            int songID = songPlaylist.getSongID();
            playlistMap.putIfAbsent(playlistID, new ArrayList<>());
            playlistMap.get(playlistID).add(songID);
        }
        return convertParentData(playlistMap);
    }

    public List<MusicExpandableListItem> convertParentData(HashMap<Integer, List<Integer>> playlistMap){
        List<MusicExpandableListItem> parentList = new ArrayList<>();
        playlistMap.forEach((playlistID, songIDList) -> {
            Playlist playlist = playlistRepository.getPlaylist(playlistID).get(0);
            List<MusicExpandableListData> childList = convertChildData(songIDList);
            parentList.add(new MusicExpandableListItem(playlist, childList));
        });
        return parentList;
    }

    public List<MusicExpandableListData> convertChildData(List<Integer> typeIDList){
        List<MusicExpandableListData> childList = new ArrayList<>();
        for(Integer typeID : typeIDList){
            Song song = songRepository.getSong(typeID).get(0);
            childList.add(new MusicExpandableListData(song));
        }
        return childList;
    }

    public LiveData<List<SongPlaylist>> getSongPlaylistList() {
        return songPlaylistList;
    }
}
