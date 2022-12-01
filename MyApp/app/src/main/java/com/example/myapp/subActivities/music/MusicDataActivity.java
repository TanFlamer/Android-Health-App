package com.example.myapp.subActivities.music;

import android.graphics.Color;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Pair;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.ListView;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databasefiles.song.Song;
import com.google.android.material.textfield.TextInputLayout;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class MusicDataActivity extends AppCompatActivity {

    MusicDataViewModel musicDataViewModel;
    ListView songSelected, songUnselected;
    Button saveButton, returnButton;
    TextInputLayout playlistNameInput;
    ImageView addImageView, removeImageView;
    EditText playlistName;

    private final int ADD_TO_PLAYLIST = 1;
    private final int REMOVE_FROM_PLAYLIST = -1;
    HashMap<Integer, Integer> changeLogs;

    MusicDataListAdapter songUnselectedAdapter;
    MusicDataListAdapter songSelectedAdapter;

    List<Pair<Song, Boolean>> unselectedSongList;
    List<Pair<Song, Boolean>> selectedSongList;

    String namePlaylist;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_music);
        musicDataViewModel = new ViewModelProvider(this).get(MusicDataViewModel.class);
        namePlaylist = musicDataViewModel.loadPlaylist(getIntent().getStringExtra("playlistName"));
        changeLogs = new HashMap<>();
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        initialiseSongLists();
        initialiseAll();
    }

    public void initialiseSongLists(){
        Pair<List<Song>, List<Song>> songLists = musicDataViewModel.populateLists();
        unselectedSongList = new ArrayList<>();
        for(Song song : songLists.first) unselectedSongList.add(new Pair<>(song, false));
        selectedSongList = new ArrayList<>();
        for(Song song : songLists.second) selectedSongList.add(new Pair<>(song, false));
    }

    public void initialiseAll(){
        initialiseListViews();
        initialiseEditText();
        initialiseImageView();
        initialiseButtons();
    }

    public void resetLists(){
        resetAdapters();
        resetImageViews();
    }

    public void resetImageViews(){
        setImageView(unselectedSongList, addImageView);
        setImageView(selectedSongList, removeImageView);
    }

    public void resetAdapters(){
        songUnselectedAdapter.notifyDataSetChanged();
        songSelectedAdapter.notifyDataSetChanged();
    }

    public boolean selected(List<Pair<Song, Boolean>> songList){
        for(Pair<Song, Boolean> pair : songList)
            if(pair.second)
                return true;
        return false;
    }

    public void initialiseListViews(){
        initialiseSongUnselectedView();
        initialiseSongSelectedView();
    }

    public void initialiseSongUnselectedView(){
        songUnselected = findViewById(R.id.songUnselected);
        songUnselected.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);
        songUnselected.setOnItemClickListener(onItemClickListener);
        songUnselectedAdapter = new MusicDataListAdapter(this, R.layout.data_music_list_item, unselectedSongList);
        songUnselected.setAdapter(songUnselectedAdapter);
    }

    public void initialiseSongSelectedView(){
        songSelected = findViewById(R.id.songSelected);
        songSelected.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);
        songSelected.setOnItemClickListener(onItemClickListener);
        songSelectedAdapter = new MusicDataListAdapter(this, R.layout.data_music_list_item, selectedSongList);
        songSelected.setAdapter(songSelectedAdapter);
    }

    public void moveSongs(List<Pair<Song, Boolean>> songListFrom, List<Pair<Song, Boolean>> songListTo, int operation){
        for(int i = songListFrom.size() - 1; i >= 0; i--){
            Pair<Song, Boolean> songBooleanPair = songListFrom.get(i);
            if(songBooleanPair.second){
                Song song = songBooleanPair.first;
                songListFrom.remove(i);
                songListTo.add(new Pair<>(song, false));
                int finalOperation = Objects.requireNonNull(changeLogs.getOrDefault(song.getSongID(), 0)) + operation;
                if(finalOperation == 0)
                    changeLogs.remove(song.getSongID());
                else
                    changeLogs.put(song.getSongID(), finalOperation);
            }
        }
    }

    public void initialiseImageView(){
        initialiseAddImageView();
        initialiseRemoveImageView();
        resetLists();
    }

    public void initialiseAddImageView(){
        addImageView = findViewById(R.id.addImageView);
        addImageView.setOnClickListener(v -> {
            moveSongs(unselectedSongList, selectedSongList, ADD_TO_PLAYLIST);
            resetLists();
            checkButton();
        });
    }

    public void initialiseRemoveImageView(){
        removeImageView = findViewById(R.id.removeImageView);
        removeImageView.setOnClickListener(v -> {
            moveSongs(selectedSongList, unselectedSongList, REMOVE_FROM_PLAYLIST);
            resetLists();
            checkButton();
        });
    }

    public boolean updateImageView(List<Pair<Song, Boolean>> songList, int position, ImageView imageView){
        Pair<Song, Boolean> songBooleanPair = songList.get(position);
        songList.set(position, new Pair<>(songBooleanPair.first, !songBooleanPair.second));
        setImageView(songList, imageView);
        return !songBooleanPair.second;
    }

    public void setImageView(List<Pair<Song, Boolean>> songList, ImageView imageView){
        boolean selected = selected(songList);
        imageView.setAlpha((float)(selected ? 1 : 0.35));
        imageView.setClickable(selected);
    }

    public void initialiseEditText(){
        playlistNameInput = findViewById(R.id.playlistNameInput);
        playlistName = findViewById(R.id.playlistName);
        playlistName.setText(namePlaylist);
        playlistName.addTextChangedListener(playlistNameTextWatcher);
        playlistName.setOnFocusChangeListener((v, hasFocus) -> validatePlaylistName(playlistNameInput, playlistName));
    }

    public void initialiseButtons(){
        saveButton = findViewById(R.id.editSaveButton);
        saveButton.setOnClickListener(v -> {
            String newPlaylistName = playlistName.getText().toString();
            if(musicDataViewModel.getPlaylist() == null)
                musicDataViewModel.insertPlaylist(newPlaylistName);
            else if(!namePlaylist.equals(newPlaylistName))
                musicDataViewModel.updatePlaylist(newPlaylistName);
            changeLogs.forEach((songID, operation) -> {
                if(operation > 0)
                    musicDataViewModel.insertSongPlaylist(songID);
                else if(operation < 0)
                    musicDataViewModel.deleteSongPlaylist(songID);
            });
            finish();
        });
        returnButton = findViewById(R.id.returnButton);
        returnButton.setOnClickListener(v -> finish());
    }

    public boolean validatePlaylistName(TextInputLayout textInputLayout, EditText editText){
        String playlistText = editText.getText().toString();
        boolean hasFocus = editText.hasFocus();
        boolean emptyPlaylistName = playlistText.isEmpty();
        boolean validPlaylistName = !emptyPlaylistName && (playlistText.equals(namePlaylist) || musicDataViewModel.validatePlaylistName(playlistText));

        if(!hasFocus || validPlaylistName)
            textInputLayout.setErrorEnabled(false);
        else if(emptyPlaylistName)
            textInputLayout.setError("Playlist name cannot be empty");
        else
            textInputLayout.setError("Playlist name already taken");
        return validPlaylistName;
    }

    public void checkButton(){
        boolean validPlaylistName = validatePlaylistName(playlistNameInput, playlistName);
        boolean emptyPlaylist = selectedSongList.isEmpty();
        boolean noChanges = changeLogs.isEmpty();
        saveButton.setEnabled(!emptyPlaylist && !noChanges && validPlaylistName);
    }

    private final TextWatcher playlistNameTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            checkButton();
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    public AdapterView.OnItemClickListener onItemClickListener = (parent, view, position, id) -> {
        boolean selected = parent.equals(songUnselected) ? updateImageView(unselectedSongList, position, addImageView) : updateImageView(selectedSongList, position, removeImageView);
        view.setBackgroundColor(selected ? Color.BLUE : Color.WHITE);
    };

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
}