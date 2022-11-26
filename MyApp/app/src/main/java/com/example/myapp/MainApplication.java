package com.example.myapp;

import android.app.Application;
import android.util.Pair;

import androidx.lifecycle.MutableLiveData;

import com.example.myapp.databaseFiles.playlist.Playlist;
import com.example.myapp.databaseFiles.playlist.PlaylistRepository;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylist;
import com.example.myapp.databaseFiles.songPlaylist.SongPlaylistRepository;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.typeSport.TypeSport;
import com.example.myapp.databaseFiles.song.SongRepository;
import com.example.myapp.databaseFiles.sport.SportRepository;
import com.example.myapp.databaseFiles.type.TypeRepository;
import com.example.myapp.databaseFiles.typeSport.TypeSportRepository;
import com.example.myapp.databaseFiles.user.UserRepository;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

public class MainApplication extends Application {

    private MutableLiveData<List<Pair<String, LocalTime>>> saveLogs;
    private int userID;

    private UserRepository userRepository;
    private SongRepository songRepository;
    private PlaylistRepository playlistRepository;
    private SongPlaylistRepository songPlaylistRepository;
    private SportRepository sportRepository;
    private TypeRepository typeRepository;
    private TypeSportRepository typeSportRepository;

    private List<Song> songList;
    private List<Playlist> playlistList;
    private List<SongPlaylist> songPlaylistList;
    private List<Type> typeList;
    private List<Sport> sportList;
    private List<TypeSport> typeSportList;

    @Override
    public void onCreate() {
        super.onCreate();
        saveLogs = new MutableLiveData<>();
        saveLogs.setValue(new ArrayList<>());
    }

    public void updateSaveLogs(Pair<String, LocalTime> newSaveLog){
        List<Pair<String, LocalTime>> oldSaveLogs = saveLogs.getValue();
        oldSaveLogs.add(newSaveLog);
        saveLogs.setValue(oldSaveLogs);
    }

    public UserRepository getUserRepository() {
        if(userRepository == null)
            userRepository = new UserRepository(this);
        return userRepository;
    }

    public SongRepository getSongRepository(){
        if(songRepository == null) {
            songList = new ArrayList<>();
            songRepository = new SongRepository(this);
            songRepository.getAllSongs(userID).observeForever(newSongList -> songList = newSongList);
        }
        return songRepository;
    }

    public PlaylistRepository getPlaylistRepository(){
        if(playlistRepository == null) {
            playlistList = new ArrayList<>();
            playlistRepository = new PlaylistRepository(this);
            playlistRepository.getAllPlaylists(userID).observeForever(newPlaylistList -> playlistList = newPlaylistList);
        }
        return playlistRepository;
    }

    public SongPlaylistRepository getSongPlaylistRepository(){
        if(songPlaylistRepository == null) {
            songPlaylistList = new ArrayList<>();
            songPlaylistRepository = new SongPlaylistRepository(this);
            songPlaylistRepository.getAllSongPlaylist(userID).observeForever(newSongPlaylistList -> songPlaylistList = newSongPlaylistList);
        }
        return songPlaylistRepository;
    }

    public SportRepository getSportRepository(){
        if(sportRepository == null) {
            sportList = new ArrayList<>();
            sportRepository = new SportRepository(this);
            sportRepository.getAllSport(userID).observeForever(newSportList -> sportList = newSportList);
        }
        return sportRepository;
    }

    public TypeRepository getTypeRepository(){
        if(typeRepository == null) {
            typeList = new ArrayList<>();
            typeRepository = new TypeRepository(this);
            typeRepository.getAllTypes(userID).observeForever(newTypeList -> typeList = newTypeList);
        }
        return typeRepository;
    }

    public TypeSportRepository getTypeSportRepository(){
        if(typeSportRepository == null) {
            typeSportList = new ArrayList<>();
            typeSportRepository = new TypeSportRepository(this);
            typeSportRepository.getAllTypeSport(userID).observeForever(newTypeSportList -> typeSportList = newTypeSportList);
        }
        return typeSportRepository;
    }

    public MutableLiveData<List<Pair<String, LocalTime>>> getSaveLogs() {
        return saveLogs;
    }

    public int getUserID() {
        return userID;
    }

    public void setUserID(int userID) {
        this.userID = userID;
    }

    public List<Song> getSongList() {
        return songList;
    }

    public List<Playlist> getPlaylistList() {
        return playlistList;
    }

    public List<SongPlaylist> getSongPlaylistList() {
        return songPlaylistList;
    }

    public List<Type> getTypeList() {
        return typeList;
    }

    public List<Sport> getSportList() {
        return sportList;
    }

    public List<TypeSport> getTypeSportList() {
        return typeSportList;
    }
}
