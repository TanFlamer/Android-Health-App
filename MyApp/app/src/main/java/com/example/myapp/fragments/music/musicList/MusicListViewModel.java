package com.example.myapp.fragments.music.musicList;

import android.Manifest;
import android.app.AlertDialog;
import android.app.Application;
import android.content.Context;
import android.content.pm.PackageManager;
import android.media.MediaMetadataRetriever;
import android.net.Uri;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.MusicPlayer;
import com.example.myapp.databasefiles.song.Song;
import com.example.myapp.databasefiles.song.SongRepository;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Comparator;
import java.util.List;

public class MusicListViewModel extends AndroidViewModel {

    private final MainApplication mainApplication;
    private final MediaMetadataRetriever mediaMetadataRetriever;
    private final SongRepository songRepository;
    private final LiveData<List<Song>> songList;
    private final MusicPlayer musicPlayer;
    private final String filePath;
    private final int userID;

    public MusicListViewModel(@NonNull Application application) {
        super(application);
        mainApplication = (MainApplication) getApplication();
        songRepository = mainApplication.getSongRepository();
        userID = mainApplication.getUserID();
        musicPlayer = mainApplication.getMusicPlayer();
        songList = songRepository.getAllSongs(userID);
        filePath = getApplication().getFilesDir() + "/music/" + userID;
        mediaMetadataRetriever = new MediaMetadataRetriever();
    }

    public LiveData<List<Song>> getSongList(){
        return songList;
    }

    public MusicPlayer getMusicPlayer() {
        return musicPlayer;
    }

    public void updateSaveLogs(String saveLogs){
        mainApplication.updateSaveLogs(saveLogs);
    }

    public void getMusicFile(ActivityResultLauncher<String> mGetContent, ActivityResultLauncher<String> requestPermissionLauncher){
        if (ContextCompat.checkSelfPermission(getApplication(), Manifest.permission.READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)
            mGetContent.launch("audio/*");
        else
            requestPermissionLauncher.launch(Manifest.permission.READ_EXTERNAL_STORAGE);
    }

    public void copyFile(Uri uri, Context context) throws IOException {
        InputStream source = context.getContentResolver().openInputStream(uri);
        String fileName = new File(uri.getPath()).getName();
        Path dest = Paths.get(filePath, fileName);
        Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING);
        checkFile(fileName, uri);
        Toast.makeText(getApplication(), fileName + " copied successfully", Toast.LENGTH_SHORT).show();
    }

    public void deleteFile(Song song){
        musicPlayer.resetMediaPlayer();
        songRepository.delete(song);
        updateSaveLogs("Song " + song.getSongName() + " deleted");
        File musicFile = new File(filePath, song.getSongName());
        boolean fileDeletion = musicFile.delete();
        Toast.makeText(getApplication(), "File deletion " + (fileDeletion ? "successful" : "failed"), Toast.LENGTH_SHORT).show();
    }

    public void checkFile(String fileName, Uri uri){
        Song song = findSong(userID, fileName);
        if(song == null) {
            songRepository.insert(new Song(fileName, getSongDuration(uri), userID));
            updateSaveLogs("Song " + fileName + " added");
        }
        else {
            songRepository.update(song);
            updateSaveLogs("Song " + fileName + " updated");
        }
    }

    public Song findSong(int userID, String songName){
        return songRepository.findSong(userID, songName);
    }

    public int getSongDuration(Uri uri){
        mediaMetadataRetriever.setDataSource(getApplication(), uri);
        String duration = mediaMetadataRetriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_DURATION);
        return Integer.parseInt(duration) / 1000;
    }

    public AlertDialog deleteSong(Context context, Song song){
        return new AlertDialog.Builder(context)
                .setTitle("Delete Item")
                .setMessage("Are you sure you want to delete this item?")
                .setPositiveButton("Yes", (dialog, which) -> deleteFile(song))
                .setNegativeButton("No", null)
                .create();
    }

    public void sortSongList(List<Song> songList, String data, String order){
        Comparator<Song> songComparator = getComparator(data, order);
        songList.sort(songComparator);
    }

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
