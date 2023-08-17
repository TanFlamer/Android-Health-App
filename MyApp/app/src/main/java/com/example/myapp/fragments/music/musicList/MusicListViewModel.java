package com.example.myapp.fragments.music.musicList;

import android.Manifest;
import android.app.AlertDialog;
import android.app.Application;
import android.content.Context;
import android.content.pm.PackageManager;
import android.media.MediaMetadataRetriever;
import android.net.Uri;
import android.os.Build;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.documentfile.provider.DocumentFile;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.MusicPlayer;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.databaseFiles.song.SongRepository;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public class MusicListViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final MediaMetadataRetriever mediaMetadataRetriever;
    private final SongRepository songRepository;
    private final LiveData<List<Song>> songList;
    private final MusicPlayer musicPlayer;
    private final String filePath;
    private final int userID;

    //constructor for music list view model
    public MusicListViewModel(@NonNull Application application) {
        super(application);
        mainApplication = getApplication();
        songRepository = mainApplication.getSongRepository();
        userID = mainApplication.getUserID();
        musicPlayer = mainApplication.getMusicPlayer();
        songList = songRepository.getAllSongs(userID);
        filePath = getApplication().getFilesDir() + "/music/" + userID;
        mediaMetadataRetriever = new MediaMetadataRetriever();
    }

    //get live data of song list
    public LiveData<List<Song>> getSongList(){
        return songList;
    }

    //get music player
    public MusicPlayer getMusicPlayer() {
        return musicPlayer;
    }

    //update any changes to logs
    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }

    //copy song from device to music folder
    public void copyFile(Uri uri, Context context) throws IOException {
        //open input stream from uri
        InputStream source = context.getContentResolver().openInputStream(uri);
        //get song file name
        String fileName = Objects.requireNonNull(DocumentFile.fromSingleUri(getApplication(), uri)).getName();
        //get file path to music folder
        Path dest = Paths.get(filePath, fileName);
        //copy song file from device to music folder
        Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING);
        //insert or update song in database depending if song already exists
        checkFile(fileName, uri);
        //show toast
        Toast.makeText(getApplication(), fileName + " copied successfully", Toast.LENGTH_SHORT).show();
    }

    //delete song from music folder
    public void deleteFile(Song song){
        //reset media player
        musicPlayer.resetMediaPlayer();
        //delete song from database
        songRepository.delete(song);
        //update logs
        updateSaveLogs("Song " + song.getSongName() + " deleted");
        //get file path to song
        File musicFile = new File(filePath, song.getSongName());
        //delete song from music folder
        boolean fileDeletion = musicFile.delete();
        //show toast
        Toast.makeText(getApplication(), "File deletion " + (fileDeletion ? "successful" : "failed"), Toast.LENGTH_SHORT).show();
    }

    //insert or update song in database depending if song already exists
    public void checkFile(String fileName, Uri uri){
        //check if song exists in database
        Song song = findSong(userID, fileName);
        if(song == null) { //if song does not exist, insert new song
            songRepository.insert(new Song(fileName, getSongDuration(uri), userID));
            updateSaveLogs("Song " + fileName + " added");
        }
        else { //if song exists
            song.setSongDuration(getSongDuration(uri)); //update song duration
            songRepository.update(song); //update song in database
            updateSaveLogs("Song " + fileName + " updated");
        }
    }

    //check if song exists in database
    public Song findSong(int userID, String songName){
        return songRepository.findSong(userID, songName);
    }

    //get song duration
    public int getSongDuration(Uri uri){
        mediaMetadataRetriever.setDataSource(getApplication(), uri);
        String duration = mediaMetadataRetriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_DURATION);
        return Integer.parseInt(duration) / 1000;
    }

    //dialog to validate song deletion
    public AlertDialog deleteSong(Context context, Song song){
        return new AlertDialog.Builder(context)
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", (dialog, which) -> deleteFile(song))
                .setNegativeButton("No", null)
                .create();
    }

    //get permission to read music files
    public void getMusicFile(ActivityResultLauncher<String> mGetContent, ActivityResultLauncher<String> requestPermissionLauncher){
        //get read media store permission if sdk >= 33 else get read external storage permission
        String permission = Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU ? Manifest.permission.READ_MEDIA_AUDIO : Manifest.permission.READ_EXTERNAL_STORAGE;
        if (ContextCompat.checkSelfPermission(getApplication(), permission) == PackageManager.PERMISSION_GRANTED)
            mGetContent.launch("audio/*"); //if permission granted, launch content getter
        else
            requestPermissionLauncher.launch(permission); //else ask for permission
    }

    //sort song list
    public void sortSongList(List<Song> songList, String data, String order){
        Comparator<Song> songComparator = getComparator(data, order);
        songList.sort(songComparator);
    }

    //return comparator to sort song list
    public Comparator<Song> getComparator(String data, String order){
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
}
