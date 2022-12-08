package com.example.myapp.mainActivities.logs;

import android.content.Context;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.R;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

public class LogsListAdapter extends ArrayAdapter<Pair<String, LocalDateTime>> {

    //logs list
    private final List<Pair<String, LocalDateTime>> saveLogs;

    //constructor for logs list adapter
    public LogsListAdapter(@NonNull Context context, int resource, List<Pair<String, LocalDateTime>> saveLogs) {
        super(context, resource, saveLogs);
        this.saveLogs = saveLogs;
    }

    @NonNull
    @Override //get view for each log
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        //inflate new view for log if null
        if(currentItemView == null) {
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.logs_list_item, parent, false);
        }

        //initialise log view data
        initialiseAll(currentItemView, getItem(position));
        //return log view
        return currentItemView;
    }

    //initialise log view data
    public void initialiseAll(View view, Pair<String, LocalDateTime> saveLog){
        //get date time format
        String formattedDateTime = saveLog.second.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        //get log text
        initialiseLogView(view, saveLog.first);
        //get log date time
        initialiseTimeView(view, formattedDateTime);
    }

    //get log text
    public void initialiseLogView(View view, String log){
        //get textview ID for log text
        TextView logView = view.findViewById(R.id.saveLog);
        //set log text
        logView.setText(log);
    }

    //get log date time
    public void initialiseTimeView(View view, String time){
        //get textview ID for log date time
        TextView timeView = view.findViewById(R.id.saveTime);
        //set log date time
        timeView.setText(time);
    }

    //update logs list when new log appears
    public void updateSaveLogs(List<Pair<String, LocalDateTime>> newSaveLogs){
        //clear old logs list
        saveLogs.clear();
        //add new logs list
        saveLogs.addAll(newSaveLogs);
        //notify adapter that dataset changed
        notifyDataSetChanged();
    }
}
