package com.example.myapp;

import android.annotation.SuppressLint;
import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.util.Pair;
import android.widget.Toast;

import androidx.lifecycle.MutableLiveData;

import com.example.myapp.databaseFiles.playlist.Playlist;
import com.example.myapp.databaseFiles.playlist.PlaylistRepository;
import com.example.myapp.databaseFiles.sleep.SleepRepository;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.databaseFiles.song.SongRepository;
import com.example.myapp.databaseFiles.songcatalogue.SongCatalogue;
import com.example.myapp.databaseFiles.songcatalogue.SongCatalogueRepository;
import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.sport.SportRepository;
import com.example.myapp.databaseFiles.sportschedule.SportSchedule;
import com.example.myapp.databaseFiles.sportschedule.SportScheduleRepository;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.type.TypeRepository;
import com.example.myapp.databaseFiles.user.UserRepository;
import com.example.myapp.mainActivities.InfoActivity;
import com.example.myapp.mainActivities.MusicActivity;
import com.example.myapp.mainActivities.SleepActivity;
import com.example.myapp.mainActivities.SportActivity;
import com.example.myapp.mainActivities.logs.LogsActivity;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

public class MainApplication extends Application {

    //live data containing save logs
    private MutableLiveData<List<Pair<String, LocalDateTime>>> saveLog;

    //current user ID
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

    //music player
    private MusicPlayer musicPlayer;

    //path to application data
    private String filePath;

    @Override //methods called on application creation
    public void onCreate() {
        super.onCreate();
        //get path to application files
        filePath = getFilesDir().toString();
        //setup app if first login
        initialSetup();
        //initialise live data for save logs
        saveLog = new MutableLiveData<>();
        //initialise music player to play songs
        musicPlayer = new MusicPlayer(this, filePath);
    }

    //method called if first login
    public void initialSetup(){
        //get shared preferences to check if first login
        SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);
        //if first login
        if(!prefs.getBoolean("setup", false)) {
            //create folder for log files
            createFolder(getApplicationContext(), "logs");
            //create folder for guest songs
            createFolder(getApplicationContext(), "music/0");
            //add creation log to guest logs
            appendLogFile(new Pair<>("GUEST account created", LocalDateTime.now()));
            //get shared preferences editor
            SharedPreferences.Editor editor = prefs.edit();
            //record first login
            editor.putBoolean("setup", true);
            //apply changes to shared preferences
            editor.apply();
            //show toast
            Toast.makeText(getApplicationContext(), "App setup successful", Toast.LENGTH_SHORT).show();
        }
    }

    //method to reset log files for new user
    public void resetLogs(){
        saveLog.setValue(new ArrayList<>());
    }

    //method to separate logs for important events like account creation or username change
    public void addLogs(String newLog){
        //add new line to log file
        separateLogFile();
        //add new log to log file
        updateSaveLogs(newLog);
    }

    //add line to log file
    public void separateLogFile(){
        try {
            //get current user log file
            String logFile = filePath + "/logs/" + userID + ".txt";
            //set file writer to append
            FileWriter fw = new FileWriter(logFile, true);
            //add new line to log file
            fw.write("\n");
            //close file writer
            fw.close();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void appendLogFile(Pair<String, LocalDateTime> stringLocalDateTimePair) {
        try {
            //get current user log file
            String logFile = filePath + "/logs/" + userID + ".txt";
            //set file writer to append
            FileWriter fw = new FileWriter(logFile, true);
            //get date time formatter
            DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
            //format current date time
            String formattedDateTime = stringLocalDateTimePair.second.format(dateTimeFormatter);
            //add new log to log file with data time
            fw.write(formattedDateTime + " " + stringLocalDateTimePair.first + "\n");
            //close file writer
            fw.close();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void createFolder(Context context, String folderName){
        //create new file with folder name
        File newFolder = new File(context.getFilesDir(), folderName);
        //create new folder
        newFolder.mkdirs();
    }

    public void updateSaveLogs(String newSaveLog){
        //create new log with current date time
        Pair<String, LocalDateTime> newLog = new Pair<>(newSaveLog, LocalDateTime.now());
        //get current log list
        List<Pair<String, LocalDateTime>> oldLogs = saveLog.getValue();
        //add new log to log list
        oldLogs.add(newLog);
        //replace old log list with new log list
        saveLog.setValue(oldLogs);
        //append new log to log file
        appendLogFile(newLog);
    }

    //create and return single instance of user repository
    public UserRepository getUserRepository() {
        if(userRepository == null)
            userRepository = new UserRepository(this);
        return userRepository;
    }

    //create and return single instance of sleep data repository
    public SleepRepository getSleepRepository(){
        if(sleepRepository == null)
            sleepRepository = new SleepRepository(this);
        return sleepRepository;
    }

    //create and return single instance of song repository and initialise list of songs
    public SongRepository getSongRepository(){
        if(songRepository == null) {
            songList = new ArrayList<>();
            songRepository = new SongRepository(this);
        }
        songRepository.getAllSongs(userID).observeForever(newSongList -> songList = newSongList);
        return songRepository;
    }

    //create and return single instance of playlist repository and initialise list of playlists
    public PlaylistRepository getPlaylistRepository(){
        if(playlistRepository == null) {
            playlistList = new ArrayList<>();
            playlistRepository = new PlaylistRepository(this);
        }
        playlistRepository.getAllPlaylists(userID).observeForever(newPlaylistList -> playlistList = newPlaylistList);
        return playlistRepository;
    }

    //create and return single instance of song catalogue repository and initialise list of song catalogues
    public SongCatalogueRepository getSongCatalogueRepository(){
        if(songCatalogueRepository == null) {
            songCatalogueList = new ArrayList<>();
            songCatalogueRepository = new SongCatalogueRepository(this);
        }
        songCatalogueRepository.getAllSongCatalogue(userID).observeForever(newSongPlaylistList -> songCatalogueList = newSongPlaylistList);
        return songCatalogueRepository;
    }

    //create and return single instance of sport data repository and initialise list of sport data
    public SportRepository getSportRepository(){
        if(sportRepository == null) {
            sportList = new ArrayList<>();
            sportRepository = new SportRepository(this);
        }
        sportRepository.getAllSport(userID).observeForever(newSportList -> sportList = newSportList);
        return sportRepository;
    }

    //create and return single instance of sport type repository and initialise list of sport type
    public TypeRepository getTypeRepository(){
        if(typeRepository == null) {
            typeList = new ArrayList<>();
            typeRepository = new TypeRepository(this);
        }
        typeRepository.getAllTypes(userID).observeForever(newTypeList -> typeList = newTypeList);
        return typeRepository;
    }

    //create and return single instance of sport schedule repository and initialise list of sport schedules
    public SportScheduleRepository getSportScheduleRepository(){
        if(sportScheduleRepository == null) {
            sportScheduleList = new ArrayList<>();
            sportScheduleRepository = new SportScheduleRepository(this);
        }
        sportScheduleRepository.getAllSportSchedule(userID).observeForever(newTypeSportList -> sportScheduleList = newTypeSportList);
        return sportScheduleRepository;
    }

    //get intent to move to different activity
    @SuppressLint("NonConstantResourceId")
    public Intent getIntent(int newActivity, int oldActivity){
        //stay in same activity if same icon pressed
        if(newActivity == oldActivity)
            return null;
        else{
            //move to different activity if different icon pressed
            switch(newActivity){
                case R.id.save:
                    return new Intent(getApplicationContext(), LogsActivity.class);

                case R.id.sleep:
                    return new Intent(getApplicationContext(), SleepActivity.class);

                case R.id.music:
                    return new Intent(getApplicationContext(), MusicActivity.class);

                case R.id.sport:
                    return new Intent(getApplicationContext(), SportActivity.class);

                case R.id.info:
                    return new Intent(getApplicationContext(), InfoActivity.class);
            }
        }
        return null;
    }

    //get live data of current save logs
    public MutableLiveData<List<Pair<String, LocalDateTime>>> getSaveLog() {
        return saveLog;
    }

    //get user ID
    public int getUserID() {
        return userID;
    }

    //set user ID
    public void setUserID(int userID) {
        this.userID = userID;
    }

    //get list of songs
    public List<Song> getSongList() {
        return songList;
    }

    //get list of playlists
    public List<Playlist> getPlaylistList() {
        return playlistList;
    }

    //get list of song catalogues
    public List<SongCatalogue> getSongCatalogueList() {
        return songCatalogueList;
    }

    //get list of sport types
    public List<Type> getTypeList() {
        return typeList;
    }

    //get list of sport data
    public List<Sport> getSportList() {
        return sportList;
    }

    //get list of sport schedules
    public List<SportSchedule> getSportScheduleList() {
        return sportScheduleList;
    }

    //get music player
    public MusicPlayer getMusicPlayer() {
        return musicPlayer;
    }
}
