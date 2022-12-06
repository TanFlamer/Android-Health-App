package com.example.myapp;

import android.app.Application;
import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.util.Pair;
import android.widget.Toast;

import androidx.lifecycle.MutableLiveData;

import com.example.myapp.databasefiles.playlist.Playlist;
import com.example.myapp.databasefiles.playlist.PlaylistRepository;
import com.example.myapp.databasefiles.sleep.SleepRepository;
import com.example.myapp.databasefiles.song.Song;
import com.example.myapp.databasefiles.song.SongRepository;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogue;
import com.example.myapp.databasefiles.songcatalogue.SongCatalogueRepository;
import com.example.myapp.databasefiles.sport.Sport;
import com.example.myapp.databasefiles.sport.SportRepository;
import com.example.myapp.databasefiles.sportschedule.SportSchedule;
import com.example.myapp.databasefiles.sportschedule.SportScheduleRepository;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.databasefiles.type.TypeRepository;
import com.example.myapp.databasefiles.user.UserRepository;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

public class MainApplication extends Application {

    private MutableLiveData<List<Pair<String, LocalDateTime>>> saveLog;
    private int userID;

    private UserRepository userRepository;
    private SleepRepository sleepRepository;
    private SongRepository songRepository;
    private PlaylistRepository playlistRepository;
    private SongCatalogueRepository songCatalogueRepository;
    private SportRepository sportRepository;
    private TypeRepository typeRepository;
    private SportScheduleRepository sportScheduleRepository;

    private List<Song> songList;
    private List<Playlist> playlistList;
    private List<SongCatalogue> songCatalogueList;
    private List<Type> typeList;
    private List<Sport> sportList;
    private List<SportSchedule> sportScheduleList;

    private MusicPlayer musicPlayer;
    private String filePath;

    @Override
    public void onCreate() {
        super.onCreate();
        filePath = getFilesDir().toString();
        initialSetup();
        saveLog = new MutableLiveData<>();
        musicPlayer = new MusicPlayer(this, filePath);
    }

    public void initialSetup(){
        SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);
        if(!prefs.getBoolean("setup", false)) {
            createFolder(getApplicationContext(), "logs");
            createFolder(getApplicationContext(), "music/0");
            appendLogFile(new Pair<>("GUEST account created", LocalDateTime.now()));
            SharedPreferences.Editor editor = prefs.edit();
            editor.putBoolean("setup", true);
            editor.apply();
            Toast.makeText(getApplicationContext(), "App setup successful", Toast.LENGTH_SHORT).show();
        }
    }

    public void resetLogs(){
        saveLog.setValue(new ArrayList<>());
    }

    public void addLogs(String newLog){
        separateLogFile();
        updateSaveLogs(newLog);
    }

    public void separateLogFile(){
        try {
            String logFile = filePath + "/logs/" + userID + ".txt";
            FileWriter fw = new FileWriter(logFile, true);
            fw.write("\n");
            fw.close();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void appendLogFile(Pair<String, LocalDateTime> stringLocalDateTimePair) {
        try {
            String logFile = filePath + "/logs/" + userID + ".txt";
            FileWriter fw = new FileWriter(logFile, true);
            DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
            String formattedDateTime = stringLocalDateTimePair.second.format(dateTimeFormatter);
            fw.write(formattedDateTime + " " + stringLocalDateTimePair.first + "\n");
            fw.close();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void createFolder(Context context, String folderName){
        File newFolder = new File(context.getFilesDir(), folderName);
        newFolder.mkdirs();
    }

    public void updateSaveLogs(String newSaveLog){
        Pair<String, LocalDateTime> newLog = new Pair<>(newSaveLog, LocalDateTime.now());
        List<Pair<String, LocalDateTime>> oldLogs = saveLog.getValue();
        oldLogs.add(newLog);
        saveLog.setValue(oldLogs);
        appendLogFile(newLog);
    }

    public UserRepository getUserRepository() {
        if(userRepository == null)
            userRepository = new UserRepository(this);
        return userRepository;
    }

    public SleepRepository getSleepRepository(){
        if(sleepRepository == null)
            sleepRepository = new SleepRepository(this);
        return sleepRepository;
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

    public SongCatalogueRepository getSongCatalogueRepository(){
        if(songCatalogueRepository == null) {
            songCatalogueList = new ArrayList<>();
            songCatalogueRepository = new SongCatalogueRepository(this);
            songCatalogueRepository.getAllSongCatalogue(userID).observeForever(newSongPlaylistList -> songCatalogueList = newSongPlaylistList);
        }
        return songCatalogueRepository;
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

    public SportScheduleRepository getSportScheduleRepository(){
        if(sportScheduleRepository == null) {
            sportScheduleList = new ArrayList<>();
            sportScheduleRepository = new SportScheduleRepository(this);
            sportScheduleRepository.getAllSportSchedule(userID).observeForever(newTypeSportList -> sportScheduleList = newTypeSportList);
        }
        return sportScheduleRepository;
    }

    public MutableLiveData<List<Pair<String, LocalDateTime>>> getSaveLog() {
        return saveLog;
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

    public List<SongCatalogue> getSongCatalogueList() {
        return songCatalogueList;
    }

    public List<Type> getTypeList() {
        return typeList;
    }

    public List<Sport> getSportList() {
        return sportList;
    }

    public List<SportSchedule> getSportScheduleList() {
        return sportScheduleList;
    }

    public MusicPlayer getMusicPlayer() {
        return musicPlayer;
    }
}
