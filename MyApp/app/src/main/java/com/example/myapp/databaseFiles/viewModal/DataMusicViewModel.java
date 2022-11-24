package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.Playlist;
import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.databaseFiles.entity.SongPlaylist;
import com.example.myapp.databaseFiles.repository.PlaylistRepository;
import com.example.myapp.databaseFiles.repository.SongPlaylistRepository;
import com.example.myapp.databaseFiles.repository.SongRepository;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DataMusicViewModel extends AndroidViewModel {

    private SongRepository songRepository;
    private PlaylistRepository playlistRepository;
    private SongPlaylistRepository songPlaylistRepository;

    private List<Song> allSongs;
    private List<Song> selectedSongList;
    private List<Song> unselectedSongList;

    private int userID;
    private int playListID;
    private String playlistName;

    public DataMusicViewModel(@NonNull Application application) {
        super(application);
        songRepository = ((MainApplication) getApplication()).getSongRepository();
        playlistRepository = new PlaylistRepository(application);
        songPlaylistRepository = new SongPlaylistRepository(application);
        allSongs = ((MainApplication) getApplication()).getSongList();
        userID = ((MainApplication) getApplication()).getUserID();
    }

    public void insertPlaylist(String newPlaylistName){
        playListID = (int) playlistRepository.insert(new Playlist(newPlaylistName, userID));
        playlistName = newPlaylistName;
    }

    public void deletePlaylist(){
        playlistRepository.delete(new Playlist(playListID, playlistName, userID));
        playListID = 0;
        playlistName = null;
    }

    public void insertSongPlaylist(int songID){
        songPlaylistRepository.insert(new SongPlaylist(playListID, songID, userID));
    }

    public void deleteSongPlaylist(int songID){
        songPlaylistRepository.delete(new SongPlaylist(playListID, songID,userID));
    }

    public boolean validatePlaylistName(String playlistName){
        return playlistRepository.findPlaylist(playlistName).size() == 0;
    }

    public Pair<List<Song>, List<Song>> populateLists(){
        if(playListID == 0) {
            unselectedSongList = new ArrayList<>(allSongs);
            selectedSongList = new ArrayList<>();
        }
        else{
            List<Song> selectedList = new ArrayList<>();
            List<SongPlaylist> songPlaylists = songPlaylistRepository.getSongPlaylist(playListID);
            for(SongPlaylist songPlaylist : songPlaylists) selectedList.add(songRepository.getSong(songPlaylist.getSongID()).get(0));

            Set<Song> selectedSong = new HashSet<>(allSongs);
            for(Song song : selectedList) selectedSong.remove(song);
            List<Song> unselectedList = new ArrayList<>(selectedSong);

            selectedSongList = new ArrayList<>(selectedList);
            unselectedSongList = new ArrayList<>(unselectedList);
        }
        return new Pair<>(unselectedSongList, selectedSongList);
    }

    public int getPlayListID() {
        return playListID;
    }

    public void setPlayListID(int playListID) {
        this.playListID = playListID;
    }

    public String getPlaylistName() {
        return playlistName;
    }

    public void setPlaylistName(String playlistName) {
        this.playlistName = playlistName;
    }
}
