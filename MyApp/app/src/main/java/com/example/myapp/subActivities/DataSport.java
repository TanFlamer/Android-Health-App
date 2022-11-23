package com.example.myapp.subActivities;

import android.app.DatePickerDialog;
import android.app.TimePickerDialog;
import android.graphics.Color;
import android.os.Bundle;
import android.util.Pair;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.ListView;
import android.widget.Spinner;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.entity.Type;
import com.example.myapp.databaseFiles.viewModal.DataSportViewModel;

import java.time.Duration;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class DataSport extends AppCompatActivity {

    DataSportViewModel dataSportViewModel;
    ListView dataSportList;
    Button buttonDate, buttonAdd, buttonDuration, buttonDelete, buttonEditSave, buttonReturn;
    Spinner typeSpinner;

    int year, month, day;
    int hour, minute;

    HashMap<Pair<Integer, Duration>, Integer> changeLogs;
    SportDataListAdapter sportDataListAdapter;
    TypeSpinnerAdapter spinnerAdapter;
    List<Pair<Type, Duration>> sportDataList;
    List<Type> typeList;
    boolean[] sportDataArray;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_sport);
        dataSportViewModel = new ViewModelProvider(this).get(DataSportViewModel.class);
        sportDataList = new ArrayList<>();
        typeList = new ArrayList<>();
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        initialiseAll();
    }

    public void populateLists(LocalDate date){
        Pair<List<Pair<Type, Duration>>, List<Type>> listPair = dataSportViewModel.populateList(date);
        sportDataList = listPair.first;
        typeList = listPair.second;
        resetList();
    }

    public void initialiseAll(){
        initialiseListView();
        initialiseSpinners();
        initialiseButtons();
        populateLists(LocalDate.of(1, 1, 1));
    }

    public void initialiseListView(){
        dataSportList = findViewById(R.id.sportDataListView);
        dataSportList.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);
        dataSportList.setOnItemClickListener(onItemClickListener);

        sportDataListAdapter = new SportDataListAdapter(this, sportDataList);
        dataSportList.setAdapter(sportDataListAdapter);
    }

    public void initialiseSpinners(){
        typeSpinner = findViewById(R.id.typeSpinner);
        typeSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                Toast.makeText(getApplicationContext(), ((Type)parent.getItemAtPosition(position)).getName() + " clicked", Toast.LENGTH_SHORT).show();
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {}
        });
        spinnerAdapter = new TypeSpinnerAdapter(this, androidx.appcompat.R.layout.support_simple_spinner_dropdown_item, typeList);
        typeSpinner.setAdapter(spinnerAdapter);
    }

    public void initialiseButtons(){
        initialiseDateButton();
        initialiseAddButton();
        initialiseDeleteButton();
        initialiseDurationButton();
        initialiseBottomButtons();
    }

    public void initialiseAddButton(){
        buttonAdd = findViewById(R.id.buttonAdd);
        buttonAdd.setOnClickListener(v -> {
            addSport();
            resetList();
        });
    }

    public void addSport(){
        Type type = (Type) typeSpinner.getSelectedItem();
        Duration duration = Duration.ofMinutes(hour * 60L + minute);

        sportDataList.add(new Pair<>(type, duration));
        typeList.remove(type);

        hour = minute = 0;

        Pair<Integer, Duration> integerDurationPair = new Pair<>(type.getTypeID(), duration);
        changeLogs.put(integerDurationPair, Objects.requireNonNull(changeLogs.getOrDefault(integerDurationPair, 0)) + 1);
    }

    public void initialiseDeleteButton(){
        buttonDelete = findViewById(R.id.buttonDelete);
        buttonDelete.setOnClickListener(v -> {
            deleteSports();
            resetList();
        });
    }

    public void initialiseDurationButton(){
        buttonDuration = findViewById(R.id.buttonDuration);
        buttonDuration.setOnClickListener(v -> new TimePickerDialog(DataSport.this, android.R.style.Theme_Holo_Light_Dialog, (timePicker, i, i2) -> {
            hour = i;
            minute = i2;
        }, hour, minute, true).show());
    }

    public void initialiseBottomButtons(){
        buttonEditSave = findViewById(R.id.buttonEditSave);
        buttonReturn = findViewById(R.id.buttonReturn);
        buttonReturn.setOnClickListener(v -> finish());
    }

    public void initialiseDateButton(){
        Calendar currentDate = Calendar.getInstance();
        year = currentDate.get(Calendar.YEAR);
        month = currentDate.get(Calendar.MONTH);
        day = currentDate.get(Calendar.DAY_OF_MONTH);

        buttonDate = findViewById(R.id.buttonDate);
        buttonDate.setOnClickListener(view -> new DatePickerDialog(this, (datePicker, i, i1, i2) -> {
            year = i;
            month = i1;
            day = i2;
            populateLists(LocalDate.of(year, month, day));
        }, year, month, day).show());
    }

    public void deleteSports(){
        for(int i = sportDataArray.length - 1; i >= 0; i--){
            if(sportDataArray[i]){
                Pair<Type, Duration> typeDurationPair = sportDataList.get(i);
                sportDataList.remove(i);
                typeList.add(typeDurationPair.first);
                Pair<Integer, Duration> integerDurationPair = new Pair<>(typeDurationPair.first.getTypeID(), typeDurationPair.second);
                changeLogs.put(integerDurationPair, Objects.requireNonNull(changeLogs.getOrDefault(integerDurationPair, 0)) - 1);
            }
        }
    }

    public void resetList(){
        sportDataListAdapter.notifyDataSetChanged();
        spinnerAdapter.notifyDataSetChanged();
        sportDataArray = new boolean[sportDataList.size()];
    }

    AdapterView.OnItemClickListener onItemClickListener = new AdapterView.OnItemClickListener() {
        @Override
        public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
            sportDataArray[position] = !sportDataArray[position];
            view.setBackgroundColor(sportDataArray[position] ? Color.BLUE : Color.WHITE);
            Pair<Type, Duration> typeDurationPair = (Pair<Type, Duration>) parent.getItemAtPosition(position);
            Toast.makeText(getApplicationContext(), typeDurationPair.first.getName() + " clicked", Toast.LENGTH_SHORT).show();
        }
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