package com.example.myappv2.recyclers;

import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myappv2.R;

public class SleepRecyclerHolder extends RecyclerView.ViewHolder {

    TextView dateView, sleepView, wakeView, durationView;

    public SleepRecyclerHolder(@NonNull View itemView) {
        super(itemView);
        dateView = itemView.findViewById(R.id.sleepDate);
        sleepView = itemView.findViewById(R.id.sleepTime);
        wakeView = itemView.findViewById(R.id.wakeTime);
        durationView = itemView.findViewById(R.id.sleepDuration);
    }
}
